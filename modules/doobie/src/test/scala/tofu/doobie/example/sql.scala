package tofu.doobie.example

import cats.data.NonEmptyList
import cats.{Apply, Monad}
import derevo.derive
import doobie.ConnectionIO
import doobie.util.log.LogHandler
import tofu.doobie.LiftConnectionIO
import tofu.doobie.example.Logging.ops.info
import tofu.doobie.log.EmbeddableLogHandler
import tofu.higherKind.Mid
import tofu.higherKind.derived.representableK
import doobie.implicits._
import tofu.doobie.example.Tracing.ops._
import tofu.syntax.monadic._

// create table department(id numeric primary key, name varchar);
// create table person(id numeric primary key, name varchar, dept_id numeric references department(id));

// Person SQL algebra
@derive(representableK)
trait PersonSql[F[_]] {
  def create(person: Person): F[Unit]
  def read(id: Long): F[Option[Person]]
}

object PersonSql {
  def make[DB[_]: Monad: LiftConnectionIO: Logging: Tracing: EmbeddableLogHandler]: PersonSql[DB] = {
    val aspects = NonEmptyList.of(new TracingMid[DB], new LoggingMid[DB]).reduce
    val impl    = EmbeddableLogHandler[DB].embedLift(implicit lh => new Impl)
    aspects attach impl
  }

  final class Impl(implicit lh: LogHandler) extends PersonSql[ConnectionIO] {
    def create(p: Person): ConnectionIO[Unit]        =
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
  def make[DB[_]: Monad: LiftConnectionIO: Logging: Tracing: EmbeddableLogHandler]: DeptSql[DB] = {
    val aspects = NonEmptyList.of(new TracingMid[DB], new LoggingMid[DB]).reduce
    val impl    = EmbeddableLogHandler[DB].embedLift(implicit lh => new Impl)
    aspects attach impl
  }

  final class Impl(implicit lh: LogHandler) extends DeptSql[ConnectionIO] {
    def create(d: Dept): ConnectionIO[Unit]        =
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
