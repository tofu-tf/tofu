package tofu.logging
package bi

import tofu.logging.ServiceLogging
import cats.Functor
import tofu.control.Bind
import scala.reflect.ClassTag
import tofu.higherKind.bi.BiMid
import tofu.higherKind.bi.FunctorBK

trait LoggingBiCompanion[U[_[_, _]]] {
  type Log[F[_, _]] = ServiceLogging[F[Nothing, *], U[Any]]

  def bimidIn[I[_]: Functor, F[+_, +_]: Bind](implicit
      logs: Logs.Safe[I, F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingBiMid],
      U: FunctorBK[U],
  ): I[U[BiMid[F, *, *]]] = Logging.bimidIn[U, I, F]
}
