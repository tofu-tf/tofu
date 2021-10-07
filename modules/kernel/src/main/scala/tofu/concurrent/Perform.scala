package tofu

import cats.tagless.{ContravariantK, FunctorK}
import tofu.internal.instances._
import tofu.internal.carriers.PerformCarrier3
import tofu.concurrent.Exit
import tofu.internal.carriers.PerformCarrier2
import cats.data.ReaderT
import cats.Functor

trait Performer[F[_], -Cont[_], Cancel] {
  def perform[A](cont: Cont[A])(fa: F[A]): F[Cancel]
}

object Performer {
  type OfExit[F[_], E] = Performer[F, PerformOf.ExitCont[E, *], Unit]

  implicit def contravariantK[F[_], Cancel]: ContravariantK[Performer[F, *[_], Cancel]] =
    new PerformerContravariantK[F, Cancel]
}

trait PerformVia[F[_], Cont[_], Cancel] extends WithContext[F, Performer[F, Cont, Cancel]] {
  def performer: F[Performer[F, Cont, Cancel]]
  final def context: F[Performer[F, Cont, Cancel]] = performer
}

object PerformVia extends PerformInterop {
  implicit def contravariantK[F[_], Cancel]: ContravariantK[PerformVia[F, *[_], Cancel]] =
    new PerformViaContravariantK[F, Cancel]

  implicit def performReader[F[_]: Functor, Cont[_], R, Cancel](implicit
      RP: PerformVia[F, Cont, Cancel]
  ): PerformVia[ReaderT[F, R, *], Cont, Cancel] = new PerformViaReader(RP)
}

class PerformInterop extends PerformInterop1 {
  final implicit def interopCE3[F[_], E](implicit carrier: PerformCarrier3[F, E]): PerformOf[F, Exit[E, *]] = carrier

}
class PerformInterop1 {
  final implicit def interopCE2[F[_], E](implicit carrier: PerformCarrier2[F, E]): PerformOf[F, Exit[E, *]] = carrier
}

trait PerformOf[F[_], Ex[_]] extends PerformVia[F, PerformOf.Cont[Ex, *], Unit]

object PerformOf {
  type Cont[Ex[_], A] = Ex[A] => Unit
  type ExitCont[E, A] = Exit[E, A] => Unit
  final implicit def functorK[F[_]]: FunctorK[PerformOf[F, *[_]]] = new PerformOfFunctorK[F]
}
