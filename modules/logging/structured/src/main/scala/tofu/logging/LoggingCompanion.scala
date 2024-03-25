package tofu
package logging

import scala.reflect.ClassTag

import cats.tagless.FunctorK
import cats.{Functor, Id, Monad}
import tofu.higherKind.{Function2K, Mid, MonoidalK}
import tofu.syntax.monadic._
import tofu.syntax.monoidalK._
import tofu.higherKind.HKAny

/** Mix-in trait that supposed to be extended by companion of service
  *
  * @example
  *   {{{class FooService[F[_] : FooService.Log] object FooService extends LoggingCompanion[FooService]}}}
  */
trait LoggingCompanion[U[_[_]]] {
  type Log[F[_]] = ServiceLogging[F, U[HKAny]]

  implicit def toLogMidOps[F[_]](uf: U[F]): LogMidOps[U, F] = new LogMidOps(uf)

  def logMid[F[_]: Monad](implicit
      L: Logging.Make[F],
      svc: ClassTag[U[F]],
      U: FunctorK[U],
      UM: LoggingMid.Of[U]
  ): U[Mid[F, _]] = Logging.mid.in[U, Id, F]

  def logMidNamed[F[_]: Monad](name: String)(implicit
      L: Logging.Make[F],
      U: FunctorK[U],
      UM: LoggingMid.Of[U]
  ): U[Mid[F, _]] = Logging.mid.named[U, Id, F](name)

  def logErrMid[F[+_]: Monad, E](implicit
      logs: Logging.Make[F],
      UCls: ClassTag[U[F]],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
      U: FunctorK[U],
  ): U[Mid[F, _]] = Logging.mid.errIn[U, Id, F, E]

  def logErrMidNamed[F[+_]: Monad, E](name: String)(implicit
      logs: Logging.Make[F],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
      U: FunctorK[U],
  ): U[Mid[F, _]] = Logging.mid.namedErr[U, Id, F, E](name)

  def logMidIn[I[_]: Functor, F[_]: Monad](implicit
      L: Logs[I, F],
      svc: ClassTag[U[F]],
      U: FunctorK[U],
      UM: LoggingMid.Of[U]
  ): I[U[Mid[F, _]]] = Logging.mid.in[U, I, F]

  def logMidNamedIn[I[_]: Functor, F[_]: Monad](name: String)(implicit
      L: Logs[I, F],
      U: FunctorK[U],
      UM: LoggingMid.Of[U]
  ): I[U[Mid[F, _]]] = Logging.mid.named[U, I, F](name)

  def logErrMidIn[I[_]: Functor, F[+_]: Monad, E](implicit
      logs: Logs[I, F],
      UCls: ClassTag[U[F]],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
      U: FunctorK[U],
  ): I[U[Mid[F, _]]] = Logging.mid.errIn[U, I, F, E]

  def logErrMidNamedIn[I[_]: Functor, F[+_]: Monad, E](name: String)(implicit
      logs: Logs[I, F],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
      U: FunctorK[U],
  ): I[U[Mid[F, _]]] = Logging.mid.namedErr[U, I, F, E](name)

}

class LogMidOps[U[f[_]], F[_]](private val uf: U[F]) extends AnyVal {
  def attachLogs(implicit
      UL: U[LoggingMid],
      cls: ClassTag[U[F]],
      L: Logging.Make[F],
      U: MonoidalK[U],
      F: Monad[F]
  ): U[F] = {
    implicit val FL: Logging[F] = L.forService[U[F]]
    UL.zipWithK(uf)(Function2K.apply[LoggingMid, F]((l, fx) => l.around(fx)))
  }

  def attachLogsNamed(name: String)(implicit
      UL: U[LoggingMid],
      L: Logging.Make[F],
      U: MonoidalK[U],
      F: Monad[F]
  ): U[F] = {
    implicit val FL: Logging[F] = L.byName(name)
    UL.zipWithK(uf)(Function2K.apply[LoggingMid, F]((l, fx) => l.around(fx)))
  }

  def attachLogsIn[I[_]](implicit
      I: Functor[I],
      UL: U[LoggingMid],
      cls: ClassTag[U[F]],
      L: Logs[I, F],
      U: MonoidalK[U],
      F: Monad[F]
  ): I[U[F]] = L.forService[U[F]].map { implicit logging =>
    UL.zipWithK(uf)(Function2K.apply[LoggingMid, F]((l, fx) => l.around(fx)))
  }

  def attachLogsNamedIn[I[_]](name: String)(implicit
      I: Functor[I],
      UL: U[LoggingMid],
      L: Logs[I, F],
      U: MonoidalK[U],
      F: Monad[F]
  ): I[U[F]] = L.byName(name).map { implicit logging =>
    UL.zipWithK(uf)(Function2K.apply[LoggingMid, F]((l, fx) => l.around(fx)))
  }

  def attachErrLogs[E](implicit
      UL: U[LoggingErrMid[E, _]],
      cls: ClassTag[U[F]],
      L: Logging.Make[F],
      U: MonoidalK[U],
      F: Monad[F],
      FE: Errors[F, E],
  ): U[F] = {
    implicit val FL: Logging[F] = L.forService[U[F]]
    UL.zipWithK(uf)(Function2K.apply[LoggingErrMid[E, _], F]((l, fx) => l.aroundErr(fx)))
  }

  def attachErrLogsNamed[E](name: String)(implicit
      UL: U[LoggingErrMid[E, _]],
      L: Logging.Make[F],
      U: MonoidalK[U],
      F: Monad[F],
      FE: Errors[F, E],
  ): U[F] = {
    implicit val FL: Logging[F] = L.byName(name)
    UL.zipWithK(uf)(Function2K.apply[LoggingErrMid[E, _], F]((l, fx) => l.aroundErr(fx)))
  }

  def attachErrLogsIn[I[_], E](implicit
      I: Functor[I],
      UL: U[LoggingErrMid[E, _]],
      cls: ClassTag[U[F]],
      L: Logs[I, F],
      U: MonoidalK[U],
      F: Monad[F],
      FE: Errors[F, E],
  ): I[U[F]] = L.forService[U[F]].map { implicit logging =>
    UL.zipWithK(uf)(Function2K.apply[LoggingErrMid[E, _], F]((l, fx) => l.aroundErr(fx)))
  }

  def attachErrLogsNamedIn[I[_], E](name: String)(implicit
      I: Functor[I],
      UL: U[LoggingErrMid[E, _]],
      L: Logs[I, F],
      U: MonoidalK[U],
      F: Monad[F],
      FE: Errors[F, E],
  ): I[U[F]] = L.byName(name).map { implicit logging =>
    UL.zipWithK(uf)(Function2K.apply[LoggingErrMid[E, _], F]((l, fx) => l.aroundErr(fx)))
  }
}
