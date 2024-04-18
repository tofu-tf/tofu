package tofu.time

import cats.data.*
import cats.{Applicative, FlatMap, Functor, Monoid}
import tofu.internal.EffectComp
import tofu.internal.instances.ClockInstance
import tofu.syntax.liftKernel.CatsTaglessLiftSyntax

import java.util.concurrent.TimeUnit

trait Clock[F[_]] {

  /** Returns the current time, as a Unix timestamp (number of time units since the Unix epoch), suspended in `F[_]`.
    *
    * This is the pure equivalent of Java's `System.currentTimeMillis`, or of `CLOCK_REALTIME` from Linux's
    * `clock_gettime()`.
    */
  def realTime(unit: TimeUnit): F[Long]

  /** Returns a monotonic clock measurement, if supported by the underlying platform.
    *
    * This is the pure equivalent of Java's `System.nanoTime`, or of `CLOCK_MONOTONIC` from Linux's `clock_gettime()`.
    */
  def nanos: F[Long]
}

object Clock extends EffectComp[Clock] with ClockInstance {
  implicit def clockForKleisli[F[_]: Clock, R]: Clock[Kleisli[F, R, _]]                            = Clock[F].lift
  implicit def clockForWriterT[F[_]: Applicative: Clock, R: Monoid]: Clock[WriterT[F, R, _]]       = Clock[F].lift
  implicit def clockForOptionT[F[_]: Functor: Clock]: Clock[OptionT[F, _]]                         = Clock[F].lift
  implicit def clockForEitherT[F[_]: Functor: Clock, E]: Clock[EitherT[F, E, _]]                   = Clock[F].lift
  implicit def clockForStateT[F[_]: Applicative: Clock, S]: Clock[StateT[F, S, _]]                 = Clock[F].lift
  implicit def clockForIorT[F[_]: Applicative: Clock, L]: Clock[IorT[F, L, _]]                     = Clock[F].lift
  implicit def clockForContT[F[_]: FlatMap: Clock, R]: Clock[ContT[F, R, _]]                       = Clock[F].lift
  implicit def clockForRWST[F[_]: Applicative: Clock, R, L: Monoid, S]: Clock[RWST[F, R, L, S, _]] = Clock[F].lift
}
