package tofu
package sim
package impl
import cats.effect.Fiber
import tofu.Start
import tofu.sim.SIM._
import tofu.syntax.monadic._
import Transact._
import cats.syntax.foldable._
import cats.instances.list._

class TofuInstance[F[+_, _]: Transact: VoidMonad: STMVMonad, E: IOMonad[F, *]] extends Start[F[RUN[E], *]] {
  def start[A](fa: F[RUN[E], A]): F[RUN[E], Fiber[F[RUN[E], *], A]] = for {
    resVar <- newTVar[F, E].of[FiberRes[E, A]](FiberRes.Working)
    id     <- exec[F, E](respondTo[A](fa)(e => resVar.write(FiberRes.fromEither(e))))
  } yield SimFiber[F, E, A](resVar, id)

  def racePair[A, B](
      fa: F[RUN[E], A],
      fb: F[RUN[E], B]
  ): F[RUN[E], Either[(A, Fiber[F[RUN[E], *], B]), (Fiber[F[RUN[E], *], A], B)]] = for {
    aVar <- newTVar[F, E].of[FiberRes[E, A]](FiberRes.Working)
    bVar <- newTVar[F, E].of[FiberRes[E, B]](FiberRes.Working)
    rVar <- newTVar[F, E].of[RaceState[E, A, B]](RaceState.Working)
    rst   = SimRace(rVar)
    aId  <- exec[F, E](
              respondTo[A](fa)(
                e => aVar.write(FiberRes.fromEither(e)),
                rst.putFirst,
              )
            )
    bId  <- exec[F, E](
              respondTo[B](fb)(
                e => bVar.write(FiberRes.fromEither(e)),
                rst.putSecond
              )
            )
    res  <- rst.get(SimFiber(aVar, aId), SimFiber(bVar, bId))
  } yield res

  def race[A, B](fa: F[RUN[E], A], fb: F[RUN[E], B]): F[RUN[E], Either[A, B]] = racePair(fa, fb).flatMap {
    case Left((a, bf))  => bf.cancel as Left(a)
    case Right((af, b)) => af.cancel as Right(b)
  }
  def never[A]: F[RUN[E], A]                                                  = fail[F, A].atomically
  def fireAndForget[A](fa: F[RUN[E], A]): F[RUN[E], Unit]                     = exec[F, E](fa.attempt.void).void

  private def respondTo[A](proc: F[RUN[E], A])(writers: (Either[E, A] => F[STM[Nothing], Unit])*): F[RUN[Nothing], Unit] =
    proc.attempt >>= (res => writers.toList.traverse_(_(res)).atomically)
}
