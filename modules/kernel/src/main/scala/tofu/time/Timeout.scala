package tofu.time

import cats.data.*
import cats.{FlatMap, Monad}
import tofu.internal.EffectComp
import tofu.internal.instances.TimeoutInstance
import tofu.lift.Unlift

import scala.concurrent.duration.FiniteDuration

trait Timeout[F[_]] {
  def timeoutTo[A](fa: F[A], after: FiniteDuration, fallback: F[A]): F[A]
}

object Timeout extends TimeoutInstance with EffectComp[Timeout] {
  implicit def timeoutForKleisli[F[_]: Monad: Timeout, R]: Timeout[Kleisli[F, R, _]] =
    timeoutUnlift[F, Kleisli[F, R, _]]

  implicit def timeoutForOptionT[F[_]: Timeout]: Timeout[OptionT[F, _]] =
    new Timeout[OptionT[F, _]] {
      override def timeoutTo[A](
          fa: OptionT[F, A],
          after: FiniteDuration,
          fallback: OptionT[F, A]
      ): OptionT[F, A] =
        OptionT(Timeout[F].timeoutTo(fa.value, after, fallback.value))
    }

  implicit def timeoutForEitherT[F[_]: Timeout, E]: Timeout[EitherT[F, E, _]] =
    new Timeout[EitherT[F, E, _]] {
      override def timeoutTo[A](
          fa: EitherT[F, E, A],
          after: FiniteDuration,
          fallback: EitherT[F, E, A]
      ): EitherT[F, E, A] =
        EitherT(Timeout[F].timeoutTo(fa.value, after, fallback.value))
    }

  implicit def timeoutForIorT[F[_]: Timeout, L]: Timeout[IorT[F, L, _]] =
    new Timeout[IorT[F, L, _]] {
      override def timeoutTo[A](
          fa: IorT[F, L, A],
          after: FiniteDuration,
          fallback: IorT[F, L, A]
      ): IorT[F, L, A] =
        IorT(Timeout[F].timeoutTo(fa.value, after, fallback.value))
    }

  implicit def timeoutForWriterT[F[_]: Timeout, L]: Timeout[WriterT[F, L, _]] =
    new Timeout[WriterT[F, L, _]] {
      override def timeoutTo[A](
          fa: WriterT[F, L, A],
          after: FiniteDuration,
          fallback: WriterT[F, L, A]
      ): WriterT[F, L, A] =
        WriterT(Timeout[F].timeoutTo(fa.run, after, fallback.run))
    }

  private def timeoutUnlift[F[_], G[_]](implicit
      timeout: Timeout[F],
      unlift: Unlift[F, G],
      G: FlatMap[G]
  ): Timeout[G[_]] =
    new Timeout[G] {
      override def timeoutTo[A](
          fa: G[A],
          after: FiniteDuration,
          fallback: G[A]
      ): G[A] =
        G.flatMap(unlift.unlift)(mapK =>
          unlift.lift(
            timeout.timeoutTo(mapK(fa), after, mapK(fallback))
          )
        )
    }
}
