package tofu

import cats.data.*
import cats.{Applicative, FlatMap, Functor, Monoid}
import tofu.internal.EffectComp
import tofu.internal.instances.DelayInstance
import tofu.syntax.liftKernel.CatsTaglessLiftSyntax

trait Delay[F[_]] {
  def delay[A](a: => A): F[A]
}

object Delay extends EffectComp[Delay] with DelayInstance {
  type Safe[F[_, _]]  = Delay[F[Nothing, _]]
  type Catch[F[_, _]] = Delay[F[Throwable, _]]

  implicit def delayForKleisli[F[_]: Delay, R]: Delay[Kleisli[F, R, _]]                            = Delay[F].lift
  implicit def delayForWriterT[F[_]: Applicative: Delay, R: Monoid]: Delay[WriterT[F, R, _]]       = Delay[F].lift
  implicit def delayForOptionT[F[_]: Functor: Delay]: Delay[OptionT[F, _]]                         = Delay[F].lift
  implicit def delayForEitherT[F[_]: Functor: Delay, E]: Delay[EitherT[F, E, _]]                   = Delay[F].lift
  implicit def delayForStateT[F[_]: Applicative: Delay, S]: Delay[StateT[F, S, _]]                 = Delay[F].lift
  implicit def delayForIorT[F[_]: Applicative: Delay, L]: Delay[IorT[F, L, _]]                     = Delay[F].lift
  implicit def delayForContT[F[_]: FlatMap: Delay, R]: Delay[ContT[F, R, _]]                       = Delay[F].lift
  implicit def delayForRWST[F[_]: Applicative: Delay, R, L: Monoid, S]: Delay[RWST[F, R, L, S, _]] = Delay[F].lift
}
