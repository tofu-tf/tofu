package tofu

import scala.util.Either
import tofu.internal.{EffectComp, Effect3Comp}
import tofu.internal.carriers.FibersCarrier2
import cats.MonadError
import tofu.internal.carriers.FibersCarrier3
import scala.annotation.unused

trait Fire[F[_]] {
  def fireAndForget[A](fa: F[A]): F[Unit]
}

object Fire extends EffectComp[Fire] with FireInstances0 {
  final implicit def byCarrierCE3[F[_], E, Ex[_], Fib[_]](implicit
      @unused FE: MonadError[F, E],
      carrier: FibersCarrier3.Aux[F, E, Ex, Fib]
  ): Fibers[F, Ex, Fib] = carrier.content
}

private[tofu] trait FireInstances0 {
  final implicit def byCarrierCE2[F[_], Ex[_], Fib[_]](implicit
      carrier: FibersCarrier2.Aux[F, Ex, Fib]
  ): Fibers[F, Ex, Fib] = carrier.content
}

trait Race[F[_]] extends Fire[F] {
  def race[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
  def never[A]: F[A]
}

object Race extends EffectComp[Race] {
  def never[F[_], A](implicit race: Race[F]): F[A] = race.never
}

trait Fibers[F[_], Exit[_], Fib[_]] extends Race[F] {
  def start[A](fa: F[A]): F[Fib[A]]
  def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(Exit[A], Fib[B]), (Fib[A], Exit[B])]]
}

object Fibers extends Effect3Comp[Fibers]
