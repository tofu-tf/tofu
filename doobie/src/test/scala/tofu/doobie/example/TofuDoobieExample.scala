package tofu.doobie.example

import cats.data.NonEmptyList
import cats.effect.{ContextShift, Effect, ExitCode}
import cats.instances.string._
import cats.tagless.syntax.functorK._
import cats.{Apply, FlatMap, Monad}
import derevo.derive
import doobie._
import doobie.implicits._
import doobie.util.log.LogHandler
import monix.eval.{Task, TaskApp}
import tofu.common.Console
import tofu.doobie.LiftConnectionIO
import tofu.doobie.example.Logging.ops._
import tofu.doobie.example.Tracing.ops._
import tofu.doobie.instances.implicits._
import tofu.doobie.log.{EmbeddableLogHandler, LogHandlerF}
import tofu.doobie.transactor.Txr
import tofu.env.Env
import tofu.higherKind.Mid
import tofu.higherKind.derived.representableK
import tofu.lift.{Lift, UnliftIO}
import tofu.syntax.console._
import tofu.syntax.context._
import tofu.syntax.lift._
import tofu.syntax.monadic._
import tofu.{HasContext, HasLocal, WithRun}

// Simple context
final case class Ctx(traceId: String)

// Naive context-aware logging
trait Logging[F[_]] {
  def info(msg: String): F[Unit]
}
object Logging      {
  def make[F[_]: FlatMap: Console: HasContext[*[_], Ctx]]: Logging[F] =
    msg => askF[F]((ctx: Ctx) => puts"[Logging][traceId=${ctx.traceId}] $msg")
  object ops {
    def info[F[_]](msg: String)(implicit F: Logging[F]): F[Unit] = F.info(msg)
  }
}

// Naive context-aware tracing
trait Tracing[F[_]] {
  def traced[A](opName: String)(fa: F[A]): F[A]
}
object Tracing      {
  def make[F[_]: FlatMap: Console: HasLocal[*[_], Ctx]]: Tracing[F] = new Tracing[F] {
    def traced[A](opName: String)(fa: F[A]): F[A] =
      askF[F]((ctx: Ctx) => puts"[Tracing][traceId=${ctx.traceId}] $opName" *> fa)
  }
  object ops {
    implicit class TracingOps[F[_], A](private val fa: F[A]) extends AnyVal {
      def traced(opName: String)(implicit F: Tracing[F]): F[A] = F.traced(opName)(fa)
    }
  }
}

// Model
final case class Person(id: Long, name: String, deptId: Long)
final case class Dept(id: Long, name: String)
// create table department(id numeric primary key, name varchar);
// create table person(id numeric primary key, name varchar, dept_id numeric references department(id));

// Person SQL algebra
@derive(representableK)
trait PersonSql[F[_]] {
  def create(person: Person): F[Unit]
  def read(id: Long): F[Option[Person]]
}

object PersonSql {
  def make[DB[_]: Monad: LiftConnectionIO: Logging: Tracing](elh: EmbeddableLogHandler[DB]): PersonSql[DB] =
    NonEmptyList.of(new LoggingMid[DB], new TracingMid[DB]).reduce attach elh.embedLift(implicit lh => new Impl)

  final class Impl(implicit lh: LogHandler) extends PersonSql[ConnectionIO] {
    def create(p: Person): ConnectionIO[Unit] =
      sql"insert into person values(${p.id}, ${p.name}, ${p.deptId})".update.run.void

    def read(id: Long): ConnectionIO[Option[Person]] =
      sql"select id, name, dept_id from person where id = $id"
        .query[Person]
        .option
  }

  final class LoggingMid[DB[_]: Apply: Logging] extends PersonSql[Mid[DB, *]] {
    def create(person: Person): Mid[DB, Unit]   = info("create person") *> _
    def read(id: Long): Mid[DB, Option[Person]] = info("read person") *> _
  }

  final class TracingMid[DB[_]: Tracing] extends PersonSql[Mid[DB, *]] {
    def create(person: Person): Mid[DB, Unit]   = _.traced("person-sql-create")
    def read(id: Long): Mid[DB, Option[Person]] = _.traced("person-sql-read")
  }
}

// Department SQL algebra
@derive(representableK)
trait DeptSql[F[_]] {
  def create(dept: Dept): F[Unit]
  def read(id: Long): F[Option[Dept]]
}

object DeptSql {
  def make[DB[_]: Monad: LiftConnectionIO: Logging: Tracing](elh: EmbeddableLogHandler[DB]): DeptSql[DB] =
    NonEmptyList.of(new LoggingMid[DB], new TracingMid[DB]).reduce attach elh.embedLift(implicit lh => new Impl)

  final class Impl(implicit lh: LogHandler) extends DeptSql[ConnectionIO] {
    def create(d: Dept): ConnectionIO[Unit] =
      sql"insert into department values(${d.id}, ${d.name})".update.run.void

    def read(id: Long): ConnectionIO[Option[Dept]] =
      sql"select id, name from department where id = $id"
        .query[Dept]
        .option
  }

  final class LoggingMid[DB[_]: Apply: Logging] extends DeptSql[Mid[DB, *]] {
    def create(dept: Dept): Mid[DB, Unit]     = info("create department") *> _
    def read(id: Long): Mid[DB, Option[Dept]] = info("read department") *> _
  }

  final class TracingMid[DB[_]: Tracing] extends DeptSql[Mid[DB, *]] {
    def create(dept: Dept): Mid[DB, Unit]     = _.traced("dept-sql-create")
    def read(id: Long): Mid[DB, Option[Dept]] = _.traced("dept-sql-read")
  }
}

// Storage algebra encapsulates database transactional logic
@derive(representableK)
trait PersonStorage[F[_]] {
  def store(person: Person, dept: Dept): F[Unit]
}

object PersonStorage {
  def make[F[_]: Apply: Logging: Tracing, DB[_]: Monad](
      persSql: PersonSql[DB],
      deptSql: DeptSql[DB],
      txr: Txr.Aux[F, DB]
  ): PersonStorage[F] =
    NonEmptyList
      .of(new LoggingMid[F], new TracingMid[F])
      .reduce attach (new Impl[DB](persSql, deptSql): PersonStorage[DB]).mapK(txr.trans)

  final class Impl[DB[_]: Monad](persSql: PersonSql[DB], deptSql: DeptSql[DB]) extends PersonStorage[DB] {
    def store(person: Person, dept: Dept): DB[Unit] =
      deptSql.create(dept) >> persSql.create(person)
  }

  final class LoggingMid[F[_]: Apply: Logging] extends PersonStorage[Mid[F, *]] {
    def store(person: Person, dept: Dept): Mid[F, Unit] = info("store dept & person") *> _
  }

  final class TracingMid[F[_]: Tracing] extends PersonStorage[Mid[F, *]] {
    def store(person: Person, dept: Dept): Mid[F, Unit] = _.traced("person-storage-store")
  }
}

object TofuDoobieExample extends TaskApp {
  def run(args: List[String]): Task[ExitCode] =
    runF[Task, Env[Ctx, *]].as(ExitCode.Success)

  def runF[I[_]: Effect: ContextShift, F[_]: Monad: Console: UnliftIO](implicit WR: WithRun[F, I, Ctx]): I[Unit] = {
    // Simplified wiring below
    implicit val loggingF: Logging[F] = Logging.make[F]
    implicit val tracingF: Tracing[F] = Tracing.make[F]

    val transactor = Transactor.fromDriverManager[I](
      driver = "org.postgresql.Driver",
      url = "jdbc:postgresql://localhost:5432/test",
      user = "postgres",
      pass = "secret"
    )
    val txr        = Txr.contextual[F](transactor)

    def initStorage[DB[_]: Monad: Console: LiftConnectionIO: Lift[F, *[_]]: HasLocal[*[_], Ctx]](
        txr: Txr.Aux[F, DB]
    ): PersonStorage[F] = {
      implicit val loggingDB: Logging[DB] = Logging.make[DB]
      implicit val tracingDB: Tracing[DB] = Tracing.make[DB]

      val lhf = LogHandlerF(ev => loggingF.info(s"SQL event: $ev"))
      val elh = EmbeddableLogHandler.async(lhf).lift[DB]

      val personSql = PersonSql.make[DB](elh)
      val deptSql   = DeptSql.make[DB](elh)

      PersonStorage.make(personSql, deptSql, txr)
    }

    val storage = initStorage[txr.DB](txr)
    val program = storage.store(Person(13L, "Alex", 42L), Dept(42L, "Marketing"))
    val launch  = WR.runContext(program)(Ctx("715a-562a-4da5-a6e0"))
    launch
  }
}
