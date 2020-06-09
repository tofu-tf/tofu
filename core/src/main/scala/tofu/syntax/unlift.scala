package tofu.syntax

import cats.effect.{CancelToken, ConcurrentEffect, Effect, ExitCase, Fiber, IO, SyncIO}
import cats.{FlatMap, Functor, ~>}
import tofu.lift.Unlift

object unlift {

  implicit final class UnliftEffectOps[F[_], G[_]](private val U: Unlift[F, G]) extends AnyVal {
    def effect(implicit G: Functor[G], E: Effect[F]): G[Effect[G]] =
      G.map(U.unlift) { unliftF =>
        new EffectInstance[F, G] {
          def toG: F ~> G           = U.liftF
          def toF: G ~> F           = unliftF
          implicit def F: Effect[F] = E
        }
      }

    def effectWith[A](f: Effect[G] => G[A])(implicit G: FlatMap[G], E: Effect[F]): G[A] =
      G.flatMap(U.unlift) { unliftF =>
        val eff = new EffectInstance[F, G] {
          def toG: F ~> G           = U.liftF
          def toF: G ~> F           = unliftF
          implicit def F: Effect[F] = E
        }
        f(eff)
      }

    def concurrentEffect(implicit G: Functor[G], CE: ConcurrentEffect[F]): G[ConcurrentEffect[G]] =
      G.map(U.unlift) { unliftF =>
        new ConcurrentEffectInstance[F, G] {
          def toG: F ~> G                     = U.liftF
          def toF: G ~> F                     = unliftF
          implicit def F: ConcurrentEffect[F] = CE
        }
      }

    def concurrentEffectWith[A](f: ConcurrentEffect[G] => G[A])(implicit G: FlatMap[G], CE: ConcurrentEffect[F]): G[A] =
      G.flatMap(U.unlift) { unliftF =>
        val ce = new ConcurrentEffectInstance[F, G] {
          def toG: F ~> G                     = U.liftF
          def toF: G ~> F                     = unliftF
          implicit def F: ConcurrentEffect[F] = CE
        }
        f(ce)
      }

  }

  private[unlift] trait EffectInstance[F[_], G[_]] extends Effect[G] {
    def toG: F ~> G
    def toF: G ~> F
    implicit def F: Effect[F]

    def pure[A](x: A): G[A] = toG(F.pure(x))

    def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B] = toG(F.flatMap(toF(ga))(a => toF(f(a))))

    def tailRecM[A, B](a: A)(f: A => G[Either[A, B]]): G[B] = toG(F.tailRecM(a)(a => toF(f(a))))

    def raiseError[A](e: Throwable): G[A] = toG(F.raiseError(e))

    def handleErrorWith[A](ga: G[A])(f: Throwable => G[A]): G[A] = toG(F.handleErrorWith(toF(ga))(t => toF(f(t))))

    def bracketCase[A, B](acquire: G[A])(use: A => G[B])(release: (A, ExitCase[Throwable]) => G[Unit]): G[B] =
      toG(F.bracketCase(toF(acquire))(a => toF(use(a)))((a, e) => toF(release(a, e))))

    def suspend[A](thunk: => G[A]): G[A] = toG(F.suspend(toF(thunk)))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): G[A] = toG(F.async(k))

    def asyncF[A](k: (Either[Throwable, A] => Unit) => G[Unit]): G[A] = toG(F.asyncF[A](cb => toF(k(cb))))

    def runAsync[A](ga: G[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] = F.runAsync(toF(ga))(cb)
  }

  private[unlift] trait ConcurrentEffectInstance[F[_], G[_]] extends EffectInstance[F, G] with ConcurrentEffect[G] {
    implicit def F: ConcurrentEffect[F]

    def start[A](ga: G[A]): G[Fiber[G, A]] = toG(F.map(F.start(toF(ga)))(_.mapK(toG)))

    def racePair[A, B](ga: G[A], gb: G[B]): G[Either[(A, Fiber[G, B]), (Fiber[G, A], B)]] =
      toG(F.map(F.racePair(toF(ga), toF(gb))) {
        case Left((a, fb))  => Left((a, fb.mapK(toG)))
        case Right((fa, b)) => Right((fa.mapK(toG), b))
      })

    def runCancelable[A](ga: G[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[G]] =
      F.runCancelable(toF(ga))(cb).map(toG(_))
  }

}
