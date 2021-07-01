package tofu

import scala.util.Either
import tofu.internal.{EffectComp, Effect3Comp}
import tofu.internal.carriers.FibersCarrier

trait Fire[F[_]] {
  def fireAndForget[A](fa: F[A]): F[Unit]
}

object Fire extends EffectComp[Fire] {
  final implicit def byCarrier[F[_], Ex[_], Fib[_]](implicit
      carrier: FibersCarrier.Aux[F, Ex, Fib]
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
  def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(A, Fib[B]), (Fib[A], B)]]
}

object Fibers extends Effect3Comp[Fibers]
