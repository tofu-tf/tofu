package tofu

import cats.MonadError
import tofu.internal.carriers.{FibersCarrier2, FibersCarrier3}
import tofu.internal.instances.FireInstance
import tofu.internal.{Effect3Comp, EffectComp}

import scala.annotation.unused
import scala.util.Either

trait Fire[F[_]] {
  def fireAndForget[A](fa: F[A]): F[Unit]
}

object Fire extends EffectComp[Fire] with FireInstance

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
