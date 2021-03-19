package tofu
package logging

import cats.Functor
import cats.Monad
import scala.reflect.ClassTag
import cats.tagless.FunctorK
import tofu.higherKind.Mid

/** Mix-in trait that supposed to be extended by companion of service
  *
  * @example {{{
  * class FooService[F[_] : FooService.Log]
  * object FooService extends LoggingCompanion[FooService]
  * }}}
  */
trait LoggingCompanion[U[_[_]]] {
  type Log[F[_]] = ServiceLogging[F, U[Any]]

  def midIn[I[_]: Functor, F[_]: Monad](implicit
      L: Logs[I, F],
      svc: ClassTag[U[F]],
      U: FunctorK[U],
      UM: LoggingMid.Of[U]
  ): I[U[Mid[F, *]]] = Logging.midIn[U, I, F]

  def errMidIn[I[_]: Functor, F[+_]: Monad, E](implicit
      logs: Logs[I, F],
      UCls: ClassTag[U[F]],
      lmid: LoggingErrMid.Of[U, E],
      errs: Errors[F, E],
      U: FunctorK[U],
  ): I[U[Mid[F, *]]] = Logging.errMidIn[U, I, F, E]
}
