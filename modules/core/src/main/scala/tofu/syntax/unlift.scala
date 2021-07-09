package tofu.syntax

import cats.effect.{Async, CancelToken, Concurrent, ConcurrentEffect, Effect, ExitCase, Fiber, IO, SyncIO}
import cats.~>
import tofu.lift.Unlift

object unlift {

  @deprecated("Unsafe, loses local context changes within G[_]", "0.9.0")
  implicit final class UnliftEffectOps[F[_], G[_]](private val U: Unlift[F, G]) extends AnyVal {
    def effect(implicit GA: Async[G], FE: Effect[F]): G[Effect[G]] =
      GA.map(U.unlift) { unliftF =>
        new EffectInstance[F, G] {
          def toG: F ~> G           = U.liftF
          def toF: G ~> F           = unliftF
          implicit def G: Async[G]  = GA
          implicit def F: Effect[F] = FE
        }
      }

    def effectWith[A](f: Effect[G] => G[A])(implicit GA: Async[G], FE: Effect[F]): G[A] =
      GA.flatMap(U.unlift) { unliftF =>
        val eff = new EffectInstance[F, G] {
          def toG: F ~> G           = U.liftF
          def toF: G ~> F           = unliftF
          implicit def G: Async[G]  = GA
          implicit def F: Effect[F] = FE
        }
        f(eff)
      }

    def concurrentEffect(implicit GC: Concurrent[G], FCE: ConcurrentEffect[F]): G[ConcurrentEffect[G]] =
      GC.map(U.unlift) { unliftF =>
        new ConcurrentEffectInstance[F, G] {
          def toG: F ~> G                     = U.liftF
          def toF: G ~> F                     = unliftF
          implicit def G: Concurrent[G]       = GC
          implicit def F: ConcurrentEffect[F] = FCE
        }
      }

    def concurrentEffectWith[A](
        f: ConcurrentEffect[G] => G[A]
    )(implicit GC: Concurrent[G], FCE: ConcurrentEffect[F]): G[A] =
      GC.flatMap(U.unlift) { unliftF =>
        val ce = new ConcurrentEffectInstance[F, G] {
          def toG: F ~> G                     = U.liftF
          def toF: G ~> F                     = unliftF
          implicit def G: Concurrent[G]       = GC
          implicit def F: ConcurrentEffect[F] = FCE
        }
        f(ce)
      }

  }

  private[unlift] trait EffectInstance[F[_], G[_]] extends Effect[G] {
    def toG: F ~> G
    def toF: G ~> F
    implicit def G: Async[G]
    implicit def F: Effect[F]

    def pure[A](x: A): G[A] = G.pure(x)

    def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B] = G.flatMap(ga)(f)

    def tailRecM[A, B](a: A)(f: A => G[Either[A, B]]): G[B] = G.tailRecM(a)(f)

    def raiseError[A](e: Throwable): G[A] = G.raiseError(e)

    def handleErrorWith[A](ga: G[A])(f: Throwable => G[A]): G[A] = G.handleErrorWith(ga)(f)

    def bracketCase[A, B](acquire: G[A])(use: A => G[B])(release: (A, ExitCase[Throwable]) => G[Unit]): G[B] =
      G.bracketCase(acquire)(use)(release)

    def suspend[A](thunk: => G[A]): G[A] = G.defer(thunk)

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): G[A] = G.async(k)

    def asyncF[A](k: (Either[Throwable, A] => Unit) => G[Unit]): G[A] = G.asyncF[A](k)

    def runAsync[A](ga: G[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] = F.runAsync(toF(ga))(cb)
  }

  private[unlift] trait ConcurrentEffectInstance[F[_], G[_]] extends EffectInstance[F, G] with ConcurrentEffect[G] {
    implicit def G: Concurrent[G]
    implicit def F: ConcurrentEffect[F]

    def start[A](ga: G[A]): G[Fiber[G, A]] = G.start(ga)

    def racePair[A, B](ga: G[A], gb: G[B]): G[Either[(A, Fiber[G, B]), (Fiber[G, A], B)]] =
      G.racePair(ga, gb)

    def runCancelable[A](ga: G[A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[CancelToken[G]] =
      F.runCancelable(toF(ga))(cb).map(toG(_))
  }

}
