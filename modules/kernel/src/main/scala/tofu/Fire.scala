package tofu

import cats.data.*
import cats.{Applicative, Functor, Monoid}
import tofu.internal.instances.{FibersInstance, FireInstance, RaceInstance}
import tofu.internal.{Effect3Comp, EffectComp}

trait Fire[F[_]] {
  def fireAndForget[A](fa: F[A]): F[Unit]
}

object Fire extends EffectComp[Fire] with FireInstance {
  implicit def fireForKleisli[F[_], R](implicit R0: Fire[F]): Fire[Kleisli[F, R, _]] =
    new KleisliFire[F, R] {
      override protected def alg: Fire[F] = R0
    }

  implicit def fireForOptionT[F[_]](implicit F0: Functor[F], R0: Fire[F]): Fire[OptionT[F, _]] =
    new OptionTFire[F] {
      override implicit protected def F: Functor[F] = F0

      override protected def alg: Fire[F] = R0
    }

  implicit def fireForEitherT[F[_], E](implicit F0: Functor[F], R0: Fire[F]): Fire[EitherT[F, E, _]] =
    new EitherTFire[F, E] {
      override implicit protected def F: Functor[F] = F0

      override protected def alg: Fire[F] = R0
    }

  implicit def fireForIorT[F[_], L](implicit F0: Applicative[F], R0: Fire[F]): Fire[IorT[F, L, _]] =
    new IorTFire[F, L] {
      override implicit protected def F: Applicative[F] = F0

      override protected def alg: Fire[F] = R0
    }

  implicit def fireForWriterT[F[_], L](implicit
      F0: Applicative[F],
      R0: Fire[F],
      L0: Monoid[L]
  ): Fire[WriterT[F, L, _]] =
    new WriterTFire[F, L] {
      override implicit protected def F: Applicative[F] = F0

      override implicit protected def L: Monoid[L] = L0

      override protected def alg: Fire[F] = R0
    }

  private[tofu] trait KleisliFire[F[_], R] extends Fire[Kleisli[F, R, _]] {
    protected def alg: Fire[F]

    override def fireAndForget[A](fa: Kleisli[F, R, A]): Kleisli[F, R, Unit] =
      Kleisli(r => alg.fireAndForget(fa.run(r)))
  }

  private[tofu] trait OptionTFire[F[_]] extends Fire[OptionT[F, _]] {
    implicit protected def F: Functor[F]
    protected def alg: Fire[F]

    override def fireAndForget[A](fa: OptionT[F, A]): OptionT[F, Unit] =
      OptionT.liftF(alg.fireAndForget(fa.value))
  }

  private[tofu] trait EitherTFire[F[_], E] extends Fire[EitherT[F, E, _]] {
    implicit protected def F: Functor[F]
    protected def alg: Fire[F]

    override def fireAndForget[A](fa: EitherT[F, E, A]): EitherT[F, E, Unit] =
      EitherT.liftF(alg.fireAndForget(fa.value))
  }

  private[tofu] trait IorTFire[F[_], L] extends Fire[IorT[F, L, _]] {
    implicit protected def F: Applicative[F]
    protected def alg: Fire[F]

    override def fireAndForget[A](fa: IorT[F, L, A]): IorT[F, L, Unit] =
      IorT.liftF(alg.fireAndForget(fa.value))
  }

  private[tofu] trait WriterTFire[F[_], L] extends Fire[WriterT[F, L, _]] {
    implicit protected def F: Applicative[F]
    implicit protected def L: Monoid[L]
    protected def alg: Fire[F]

    override def fireAndForget[A](fa: WriterT[F, L, A]): WriterT[F, L, Unit] =
      WriterT.liftF(alg.fireAndForget(fa.run))
  }
}

trait Race[F[_]] extends Fire[F] {
  def race[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
  def never[A]: F[A]
}

object Race extends EffectComp[Race] with RaceInstance {
  def never[F[_], A](implicit race: Race[F]): F[A] = race.never

  implicit def raceForKleisli[F[_], R](implicit R0: Race[F]): Race[ReaderT[F, R, _]] =
    new KleisliRace[F, R] {
      override protected def alg: Race[F] = R0
    }

  implicit def raceForOptionT[F[_]](implicit F0: Functor[F], R0: Race[F]): Race[OptionT[F, _]] =
    new OptionTRace[F] {
      override implicit protected def F: Functor[F] = F0

      override protected def alg: Race[F] = R0
    }

  implicit def raceForEitherT[F[_], E](implicit F0: Functor[F], R0: Race[F]): Race[EitherT[F, E, _]] =
    new EitherTRace[F, E] {
      override implicit protected def F: Functor[F] = F0

      override protected def alg: Race[F] = R0
    }

  implicit def raceForIorT[F[_], L](implicit F0: Applicative[F], R0: Race[F]): Race[IorT[F, L, _]] =
    new IorTRace[F, L] {
      override implicit protected def F: Applicative[F] = F0

      override protected def alg: Race[F] = R0
    }

  implicit def raceForWriterT[F[_], L](implicit
      F0: Applicative[F],
      R0: Race[F],
      L0: Monoid[L]
  ): Race[WriterT[F, L, _]] =
    new WriterTRace[F, L] {
      override implicit protected def F: Applicative[F] = F0

      override implicit protected def L: Monoid[L] = L0

      override protected def alg: Race[F] = R0
    }

  private[tofu] trait KleisliRace[F[_], R] extends Race[Kleisli[F, R, _]] with Fire.KleisliFire[F, R] {
    protected def alg: Race[F]

    override def race[A, B](fa: Kleisli[F, R, A], fb: Kleisli[F, R, B]): Kleisli[F, R, Either[A, B]] =
      Kleisli(r => alg.race(fa.run(r), fb.run(r)))

    override def never[A]: Kleisli[F, R, A] =
      Kleisli.liftF(alg.never)
  }

  private[tofu] trait OptionTRace[F[_]] extends Race[OptionT[F, _]] with Fire.OptionTFire[F] {
    implicit protected def F: Functor[F]
    protected def alg: Race[F]

    override def race[A, B](fa: OptionT[F, A], fb: OptionT[F, B]): OptionT[F, Either[A, B]] =
      OptionT(F.map(alg.race(fa.value, fb.value)) {
        case Left(value)  => value.map(Left(_))
        case Right(value) => value.map(Right(_))
      })

    override def never[A]: OptionT[F, A] =
      OptionT.liftF(alg.never[A])
  }

  private[tofu] trait EitherTRace[F[_], E] extends Race[EitherT[F, E, _]] with Fire.EitherTFire[F, E] {
    implicit protected def F: Functor[F]
    protected def alg: Race[F]

    override def race[A, B](fa: EitherT[F, E, A], fb: EitherT[F, E, B]): EitherT[F, E, Either[A, B]] =
      EitherT(F.map(alg.race(fa.value, fb.value)) {
        case Left(value)  => value.map(Left(_))
        case Right(value) => value.map(Right(_))
      })

    override def never[A]: EitherT[F, E, A] =
      EitherT.liftF(alg.never[A])
  }

  private[tofu] trait IorTRace[F[_], L] extends Race[IorT[F, L, _]] with Fire.IorTFire[F, L] {
    implicit protected def F: Applicative[F]
    protected def alg: Race[F]

    override def race[A, B](fa: IorT[F, L, A], fb: IorT[F, L, B]): IorT[F, L, Either[A, B]] =
      IorT(F.map(alg.race(fa.value, fb.value)) {
        case Left(value)  => value.map(Left(_))
        case Right(value) => value.map(Right(_))
      })

    override def never[A]: IorT[F, L, A] =
      IorT.liftF(alg.never[A])
  }

  private[tofu] trait WriterTRace[F[_], L] extends Race[WriterT[F, L, _]] with Fire.WriterTFire[F, L] {
    implicit protected def F: Applicative[F]
    implicit protected def L: Monoid[L]
    protected def alg: Race[F]

    override def race[A, B](fa: WriterT[F, L, A], fb: WriterT[F, L, B]): WriterT[F, L, Either[A, B]] =
      WriterT(F.map(alg.race(fa.run, fb.run)) {
        case Left(value)  => value.copy(_2 = Left(value._2))
        case Right(value) => value.copy(_2 = Right(value._2))
      })

    override def never[A]: WriterT[F, L, A] =
      WriterT.liftF(alg.never[A])
  }
}

trait Fibers[F[_], Exit[_], Fib[_]] extends Race[F] {
  def start[A](fa: F[A]): F[Fib[A]]
  def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(Exit[A], Fib[B]), (Fib[A], Exit[B])]]
}

object Fibers extends Effect3Comp[Fibers] with FibersInstance
