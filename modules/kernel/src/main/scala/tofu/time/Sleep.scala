package tofu.time

import cats.data.*
import cats.{Applicative, FlatMap, Functor, Monoid}
import tofu.internal.EffectComp
import tofu.internal.instances.SleepInstance
import tofu.syntax.liftKernel.CatsTaglessLiftSyntax

import scala.concurrent.duration.FiniteDuration

trait Sleep[F[_]] {

  /** Pauses execution for desired duration
    */
  def sleep(duration: FiniteDuration): F[Unit]
}

object Sleep extends SleepInstance with EffectComp[Sleep] {
  implicit def sleepForKleisli[F[_]: Sleep, R]: Sleep[Kleisli[F, R, _]]                            = Sleep[F].lift
  implicit def sleepForWriterT[F[_]: Applicative: Sleep, R: Monoid]: Sleep[WriterT[F, R, _]]       = Sleep[F].lift
  implicit def sleepForOptionT[F[_]: Functor: Sleep]: Sleep[OptionT[F, _]]                         = Sleep[F].lift
  implicit def sleepForEitherT[F[_]: Functor: Sleep, E]: Sleep[EitherT[F, E, _]]                   = Sleep[F].lift
  implicit def sleepForStateT[F[_]: Applicative: Sleep, S]: Sleep[StateT[F, S, _]]                 = Sleep[F].lift
  implicit def sleepForIorT[F[_]: Applicative: Sleep, L]: Sleep[IorT[F, L, _]]                     = Sleep[F].lift
  implicit def sleepForContT[F[_]: FlatMap: Sleep, R]: Sleep[ContT[F, R, _]]                       = Sleep[F].lift
  implicit def sleepForRWST[F[_]: Applicative: Sleep, R, L: Monoid, S]: Sleep[RWST[F, R, L, S, _]] = Sleep[F].lift
}
