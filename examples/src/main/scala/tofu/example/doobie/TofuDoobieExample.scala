package tofu.example.doobie

import cats.data.ReaderT
import cats.effect.{ContextShift, IO, IOApp, Sync, Effect}
import cats.tagless.syntax.functorK._
import cats.{Apply, Monad}
import derevo.derive
import doobie._
import doobie.implicits._
import doobie.util.log.LogHandler
import tofu.doobie.LiftConnectionIO
import tofu.doobie.log.{EmbeddableLogHandler, LogHandlerF}
import tofu.doobie.transactor.Txr
import tofu.higherKind.derived.representableK
import tofu.kernel.types.PerformThrow
import tofu.lift.Lift
import tofu.logging.derivation.{loggable, loggingMidTry}
import tofu.logging.{Logging, LoggingCompanion}
import tofu.syntax.context._
import tofu.syntax.doobie.log.handler._
import tofu.syntax.doobie.log.string._
import tofu.syntax.monadic._
import tofu.{Delay, Tries, WithLocal, WithRun}

// Simple context
@derive(loggable)
final case class Ctx(traceId: String)

// Model
@derive(loggable)
final case class Person(id: Long, name: String, deptId: Long)

@derive(loggable)
final case class Dept(id: Long, name: String)

// Person SQL algebra
@derive(representableK, loggingMidTry)
trait PersonSql[F[_]] {
  def init: F[Unit]
  def create(person: Person): F[Unit]
  def read(id: Long): F[Option[Person]]
}

object PersonSql extends LoggingCompanion[PersonSql] {
  def make[DB[_]: Monad: LiftConnectionIO: EmbeddableLogHandler]: PersonSql[DB] = {
    EmbeddableLogHandler[DB].embedLift(implicit lh => new Impl)
  }

  final class Impl(implicit lh: LogHandler) extends PersonSql[ConnectionIO] {
    def init: ConnectionIO[Unit]                     =
      lsql"create table if not exists person(id int8, name varchar(50), dept_id int8)".update.run.void
    def create(p: Person): ConnectionIO[Unit]        =
      lsql"insert into person values(${p.id}, ${p.name}, ${p.deptId})".update.run.void
    def read(id: Long): ConnectionIO[Option[Person]] =
      lsql"select id, name, dept_id from person where id = $id"
        .query[Person]
        .option
  }
}

// Department SQL algebra
@derive(representableK, loggingMidTry)
trait DeptSql[F[_]] {
  def init: F[Unit]
  def create(dept: Dept): F[Unit]
  def read(id: Long): F[Option[Dept]]
}

object DeptSql extends LoggingCompanion[DeptSql] {
  def make[DB[_]: Monad: LiftConnectionIO: EmbeddableLogHandler]: DeptSql[DB] = {
    EmbeddableLogHandler[DB].embedLift(implicit lh => new Impl)
  }

  final class Impl(implicit lh: LogHandler) extends DeptSql[ConnectionIO] {
    def init: ConnectionIO[Unit]                   =
      lsql"create table if not exists department(id int8, name varchar(50))".update.run.void
    def create(d: Dept): ConnectionIO[Unit]        =
      lsql"insert into department values(${d.id}, ${d.name})".update.run.void
    def read(id: Long): ConnectionIO[Option[Dept]] =
      lsql"select id, name from department where id = $id"
        .query[Dept]
        .option
  }
}

// Storage algebra encapsulates database transactional logic
@derive(representableK, loggingMidTry)
trait PersonStorage[F[_]] {
  def init: F[Unit]
  def store(person: Person, dept: Dept): F[Unit]
}

object PersonStorage extends LoggingCompanion[PersonStorage] {
  def make[F[_]: Apply, DB[_]: Monad: Txr[F, *[_]]](
      persSql: PersonSql[DB],
      deptSql: DeptSql[DB]
  ): PersonStorage[F] = {
    val impl = new Impl[DB](persSql, deptSql): PersonStorage[DB]
    val tx   = Txr[F, DB].trans
    impl.mapK(tx)
  }

  final class Impl[DB[_]: Monad](persSql: PersonSql[DB], deptSql: DeptSql[DB]) extends PersonStorage[DB] {
    def init: DB[Unit]                              =
      deptSql.init >> persSql.init
    def store(person: Person, dept: Dept): DB[Unit] =
      deptSql.create(dept) >> persSql.create(person)
  }
}

object TofuDoobieExample extends IOApp.Simple {
  val run: IO[Unit] = runF[IO, ReaderT[IO, Ctx, *]]

  def runF[I[_]: Async: ContextShift, F[_]: Sync: PerformThrow: WithRun[*[_], I, Ctx]]: I[Unit] = {
    // Simplified wiring below
    implicit val loggingF = Logging.Make.contextual[F, Ctx]

    val transactor   = Transactor.fromDriverManager[I](
      driver = "org.h2.Driver",
      url = "jdbc:h2:./test"
    )
    implicit val txr = Txr.continuational(transactor.mapK(Lift.trans[I, F]))

    def initStorage[
        DB[_]: Tries: Txr[F, *[_]]: Delay: Monad: LiftConnectionIO: WithLocal[*[_], Ctx]: PerformThrow
    ]: PersonStorage[F] = {
      implicit val loggingDB = Logging.Make.contextual[DB, Ctx]

      implicit val elh = EmbeddableLogHandler.sync(LogHandlerF.loggable[DB](Logging.Debug))

      val personSql = PersonSql.make[DB].attachErrLogs
      val deptSql   = DeptSql.make[DB].attachErrLogs

      PersonStorage.make[F, DB](personSql, deptSql).attachErrLogs
    }

    val storage = initStorage[txr.DB]
    val program = storage.init >> storage.store(Person(13L, "Alex", 42L), Dept(42L, "Marketing"))
    val launch  = runContext(program)(Ctx("715a-562a-4da5-a6e0"))
    launch
  }
}
