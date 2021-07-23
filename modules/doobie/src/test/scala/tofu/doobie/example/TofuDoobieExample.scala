package tofu.doobie.example

import cats.data.NonEmptyList
import cats.effect.{ContextShift, Effect, ExitCode, Sync}
import cats.tagless.syntax.functorK._
import cats.{Apply, Monad}
import derevo.derive
import doobie._
import monix.eval.{Task, TaskApp}
import tofu.common.Console
import tofu.doobie.LiftConnectionIO
import tofu.doobie.example.Logging.ops._
import tofu.doobie.example.Tracing.ops._
import tofu.doobie.log.{EmbeddableLogHandler, LogHandlerF}
import tofu.doobie.transactor.Txr
import tofu.env.Env
import tofu.higherKind.Mid
import tofu.higherKind.derived.representableK
import tofu.lift.UnliftIO
import tofu.syntax.monadic._
import tofu.{ WithLocal, WithRun}


@derive(representableK)
trait PersonStorage[F[_]] {
  def store(person: Person, dept: Dept): F[Unit]
}

object PersonStorage {
  def make[F[_]: Apply: Logging: Tracing, DB[_]: Monad: Txr[F, *[_]]](
      persSql: PersonSql[DB],
      deptSql: DeptSql[DB]
  ): PersonStorage[F] = {
    val aspects = NonEmptyList.of(new TracingMid[F], new LoggingMid[F]).reduce
    val impl    = new Impl[DB](persSql, deptSql): PersonStorage[DB]
    val tx      = Txr[F, DB].trans
    aspects attach impl.mapK(tx)
  }

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

  def runF[I[_]: Effect: ContextShift, F[_]: Sync: UnliftIO](implicit WR: WithRun[F, I, Ctx]): I[Unit] = {
    // Simplified wiring below
    implicit val loggingF = Logging.make[F]
    implicit val tracingF = Tracing.make[F]

    val transactor   = Transactor.fromDriverManager[I](
      driver = "org.postgresql.Driver",
      url = "jdbc:postgresql://localhost:5432/test",
      user = "postgres",
      pass = "secret"
    )
    implicit val txr = Txr.continuational(transactor.mapK(WR.liftF))

    def initStorage[
        DB[_]: Txr[F, *[_]]: Monad: Console: LiftConnectionIO: WithLocal[*[_], Ctx]: UnliftIO
    ]: PersonStorage[F] = {
      implicit val loggingDB = Logging.make[DB]
      implicit val tracingDB = Tracing.make[DB]

      val lhf          = LogHandlerF(ev => loggingDB.info(s"SQL event: $ev"))
      implicit val elh = EmbeddableLogHandler.sync(lhf)

      val personSql = PersonSql.make[DB]
      val deptSql   = DeptSql.make[DB]

      PersonStorage.make(personSql, deptSql)
    }

    val storage = initStorage
    val program = storage.store(Person(13L, "Alex", 42L), Dept(42L, "Marketing"))
    val launch  = WR.runContext(program)(Ctx("715a-562a-4da5-a6e0"))
    launch
  }
}
