package tofu.logging.internal

import cats.data.Tuple2K
import cats.kernel.Monoid
import cats.tagless.{ApplyK, FunctorK}
import cats.{Applicative, Apply, Functor, Id, ~>}
import tofu.higherKind
import tofu.higherKind.{Function2K, MonoidalK, Point, RepresentableK}
import tofu.logging.{Logging, Logs}
import cats.tagless.syntax.functorK._
import tofu.syntax.monoidalK._
import tofu.syntax.monadic._

import scala.reflect.ClassTag

trait LogsInstances {

  implicit def logsMonoid[I[_]: Applicative, F[_]: Applicative]: Monoid[Logs[I, F]] = new Monoid[Logs[I, F]] {
    def empty: Logs[I, F]                                 = Logs.empty[I, F]
    def combine(x: Logs[I, F], y: Logs[I, F]): Logs[I, F] = Logs.combine(x, y)
  }

  private[this] val logs1RepresentableAny: RepresentableK[Logs[*[_], Any]] =
    higherKind.derived.genRepresentableK[Logs[*[_], Any]]

  implicit def logs1Representable[Y[_]]: RepresentableK[Logs[*[_], Y]] =
    logs1RepresentableAny.asInstanceOf[RepresentableK[Logs[*[_], Y]]]

  implicit val logs2UniversalRepresentable: RepresentableK[Logs[Id, *[_]]] =
    higherKind.derived.genRepresentableK[Logs[Id, *[_]]]

  implicit def logs2MonoidalK[Y[_]](implicit Y: Applicative[Y]): MonoidalK[Logs[Y, *[_]]] =
    new Logs2MonoidalK[Y] { def I: Applicative[Y] = Y }
}

private[logging] trait LogsInstances0 extends LogsInstances1 {
  implicit def logs2ApplyK[Y[_]](implicit Y: Apply[Y]): ApplyK[Logs[Y, *[_]]] =
    new Logs2ApplyK[Y] { def I: Apply[Y] = Y }
}

private[logging] trait LogsInstances1 {
  implicit def logs2FunctorK[Y[_]](implicit Y: Functor[Y]): FunctorK[Logs[Y, *[_]]] =
    new Logs2FunctorK[Y] { def I: Functor[Y] = Y }
}

trait Logs2FunctorK[Y[_]] extends FunctorK[Logs[Y, *[_]]] {
  implicit def I: Functor[Y]

  def mapK[F[_], G[_]](af: Logs[Y, F])(fk: F ~> G): Logs[Y, G] = new Logs[Y, G] {
    override def forService[Svc: ClassTag]: Y[Logging[G]] = af.forService[Svc].map(_.mapK(fk))
    def byName(name: String): Y[Logging[G]]               = af.byName(name).map(_.mapK(fk))
  }
}

trait Logs2ApplyK[Y[_]] extends Logs2FunctorK[Y] with ApplyK[Logs[Y, *[_]]] {
  implicit def I: Apply[Y]

  def zipWith2K[F[_], G[_], H[_]](af: Logs[Y, F], ag: Logs[Y, G])(f2: Function2K[F, G, H]): Logs[Y, H] =
    new Logs[Y, H] {
      override def forService[Svc: ClassTag]: Y[Logging[H]] =
        (af.forService[Svc], ag.forService[Svc]).mapN(_.zipWithK(_)(f2))
      def byName(name: String): Y[Logging[H]]               = (af.byName(name), ag.byName(name)).mapN(_.zipWithK(_)(f2))
    }

  def productK[F[_], G[_]](af: Logs[Y, F], ag: Logs[Y, G]): Logs[Y, Tuple2K[F, G, *]] =
    zipWith2K(af, ag)(Function2K((f, g) => Tuple2K(f, g)))
}

trait Logs2MonoidalK[Y[_]] extends Logs2ApplyK[Y] with MonoidalK[Logs[Y, *[_]]] {
  implicit def I: Applicative[Y]

  def pureK[F[_]](p: Point[F]): Logs[Y, F] = new Logs[Y, F] {
    def byName(name: String): Y[Logging[F]] = p.pureK[Logging].pure[Y]
  }
}
