package examples.doobie

import cats.data.NonEmptyList
import cats.effect.{Effect, ExitCode}
import cats.{Apply, Monad}
import cats.tagless.syntax.functorK._
import derevo.derive
import doobie._
import doobie.implicits._
import doobie.util.log.LogHandler
import monix.eval.{Task, TaskApp}
import simulacrum.typeclass
import tofu.doobie.LiftConnectionIO
import tofu.doobie.log.{EmbeddableLogHandler, LogHandlerF}
import tofu.doobie.transactor.Txr
import tofu.doobie.instances.implicits._
import tofu.env.Env
import tofu.higherKind.Mid
import tofu.higherKind.derived.representableK
import tofu.lift.{Lift, UnliftIO}
import tofu.syntax.lift._
import tofu.syntax.monadic._
import tofu.{WithProvide, WithRun}

@derive(representableK)
trait Logging[F[_]] {
  def info(msg: String): F[Unit]
}

@typeclass
trait Tracing[F[_]] {
  def traced[A](fa: F[A])(opName: String): F[A]
}
import Tracing.ops._

final case class Person(id: Long, name: String, deptId: Long)
final case class Dept(id: Long, title: String)

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

  final class TracingMid[DB[_]: Tracing] extends PersonSql[Mid[DB, *]] {
    def create(person: Person): Mid[DB, Unit]   = _.traced("person-sql-create")
    def read(id: Long): Mid[DB, Option[Person]] =
      _.traced("person-sql-read")
  }

  final class LoggingMid[DB[_]: Apply](implicit L: Logging[DB]) extends PersonSql[Mid[DB, *]] {
    def create(person: Person): Mid[DB, Unit]   = L.info("create person") *> _
    def read(id: Long): Mid[DB, Option[Person]] = L.info("read person") *> _
  }
}

@derive(representableK)
trait DeptSql[F[_]] {
  def create(dept: Dept): F[Unit]
  def read(id: Long): F[Option[Dept]]
}

object DeptSql {
  def make[DB[_]: Monad: LiftConnectionIO: Logging: Tracing](elh: EmbeddableLogHandler[DB]): DeptSql[DB] = ???
  // some implementation similar to PersonSql
}

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
      // transactional logic
      deptSql.create(dept) >> persSql.create(person)
  }

  final class TracingMid[F[_]: Tracing] extends PersonStorage[Mid[F, *]] {
    def store(person: Person, dept: Dept): Mid[F, Unit] =
      _.traced("person-storage-store")
  }

  final class LoggingMid[F[_]: Apply](implicit L: Logging[F]) extends PersonStorage[Mid[F, *]] {
    def store(person: Person, dept: Dept): Mid[F, Unit] =
      L.info("store dept & person") *> _
  }

}

object TofuDoobieExample extends TaskApp {

  final case class Ctx()

  def run(args: List[String]): Task[ExitCode] =
    runF[Task, Env[Ctx, *]].as(ExitCode.Success)

  def runF[I[_]: Effect, F[_]: Monad: WithRun[*[_], I, Ctx]: UnliftIO]: I[Unit] = {
    // vals below are for simplification
    // it may be a BM4-flavored `for` with effectful initialization
    // or distage modules
    implicit val tracing: Tracing[F] = ???
    implicit val logging: Logging[F] = ???

    val transactor: Transactor[I] = ???
    val txr                       = Txr.contextual[F](transactor)

    def initWithDB[DB[_]: Monad: LiftConnectionIO: Lift[F, *[_]]](txr: Txr.Aux[F, DB]): PersonStorage[F] = {
      val elh = EmbeddableLogHandler.async(LogHandlerF(ev => logging.info(s"SQL event: $ev"))).lift[DB]

      implicit val tracingDB: Tracing[DB] = ???
      implicit val loggingDB: Logging[DB] = ???

      val personSql     = PersonSql.make[DB](elh)
      val deptSql       = DeptSql.make[DB](elh)
      val personStorage = PersonStorage.make(personSql, deptSql, txr)
      personStorage
    }

    val storage = initWithDB[txr.DB](txr)

    val program = storage.store(Person(1L, "Alex", 22L), Dept(22L, "Marketing"))

    val res = WithProvide[F, I, Ctx].runContext(program)(Ctx())

    res
  }
}
