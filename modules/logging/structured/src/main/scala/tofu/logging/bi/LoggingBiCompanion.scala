package tofu.logging
package bi

import scala.reflect.ClassTag

import cats.{Functor, Id}
import tofu.control.Bind
import tofu.higherKind.bi.{BiMid, Fun2BK, FunctorBK, MonoidalBK}
import tofu.syntax.functorbk._
import tofu.syntax.monadic._

trait LoggingBiCompanion[U[_[_, _]]] {
  type Log[F[_, _]] = ServiceLogging[F[Nothing, _], U[({ type L[_, _] = Any })#L]]

  implicit def toBiLogBiMidOps[F[+_, +_]](uf: U[F]): LogBiMidOps[U, F] = new LogBiMidOps(uf)

  def bimid[F[+_, +_]: Bind](implicit
      logs: Logs.SafeUniversal[F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingBiMid],
      U: FunctorBK[U],
  ): U[BiMid[F, _, _]] = Logging.mid.inBi[U, Id, F]

  def bimidNamed[F[+_, +_]: Bind](name: String)(implicit
      logs: Logs.SafeUniversal[F],
      lmid: U[LoggingBiMid],
      U: FunctorBK[U],
  ): U[BiMid[F, _, _]] = Logging.mid.namedBi[U, Id, F](name)

  def bimidIn[I[_]: Functor, F[+_, +_]: Bind](implicit
      logs: Logs.Safe[I, F],
      UCls: ClassTag[U[F]],
      lmid: U[LoggingBiMid],
      U: FunctorBK[U],
  ): I[U[BiMid[F, _, _]]] = Logging.mid.inBi[U, I, F]

  def bimidNamedIn[I[_]: Functor, F[+_, +_]: Bind](name: String)(implicit
      logs: Logs.Safe[I, F],
      lmid: U[LoggingBiMid],
      U: FunctorBK[U],
  ): I[U[BiMid[F, _, _]]] = Logging.mid.namedBi[U, I, F](name)
}

class LogBiMidOps[U[f[_, _]], F[+_, +_]](private val uf: U[F]) extends AnyVal {
  def attachLogs(implicit
      logs: Logs.SafeUniversal[F],
      UCls: ClassTag[U[F]],
      UL: U[LoggingBiMid],
      U: MonoidalBK[U],
      F: Bind[F],
  ): U[F] = {
    implicit val logging: Logging.Safe[F] = logs.forService[U[F]]
    UL.map2b(uf)(Fun2BK.apply[LoggingBiMid, F]((l, fx) => l.around(fx)))
  }

  def attachLogsIn[I[_]: Functor](implicit
      logs: Logs.Safe[I, F],
      UCls: ClassTag[U[F]],
      UL: U[LoggingBiMid],
      U: MonoidalBK[U],
      F: Bind[F],
  ): I[U[F]] = logs.forService[U[F]].map { implicit logging =>
    UL.map2b(uf)(Fun2BK.apply[LoggingBiMid, F]((l, fx) => l.around(fx)))
  }

  def attachLogsNamed(name: String)(implicit
      logs: Logs.SafeUniversal[F],
      UL: U[LoggingBiMid],
      U: MonoidalBK[U],
      F: Bind[F],
  ): U[F] = {
    implicit val logging: Logging.Safe[F] = logs.byName(name)
    UL.map2b(uf)(Fun2BK.apply[LoggingBiMid, F]((l, fx) => l.around(fx)))
  }

  def attachLogsNamedIn[I[_]: Functor](name: String)(implicit
      logs: Logs.Safe[I, F],
      UL: U[LoggingBiMid],
      U: MonoidalBK[U],
      F: Bind[F],
  ): I[U[F]] = logs.byName(name).map { implicit logging =>
    UL.map2b(uf)(Fun2BK.apply[LoggingBiMid, F]((l, fx) => l.around(fx)))
  }
}
