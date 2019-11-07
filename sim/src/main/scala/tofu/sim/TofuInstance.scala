package tofu
package sim
import cats.effect.Fiber
import tofu.Start
import tofu.sim.SIM.{IOMonad, RUN}
import tofu.syntax.monadic._
import Transact._

class TofuInstance[F[_, _]: Transact: IOMonad[*[_, _], Void], E: IOMonad[F, *]] extends Start[F[RUN[E], *]] {
  def start[A](fa: F[RUN[E], A]): F[RUN[E], Fiber[F[RUN[E], *], A]] = ???
  def racePair[A, B](
      fa: F[RUN[E], A],
      fb: F[RUN[E], B]
  ): F[RUN[E], Either[(A, Fiber[F[RUN[E], *], B]), (Fiber[F[RUN[E], *], A], B)]] = ???
  def race[A, B](fa: F[RUN[E], A], fb: F[RUN[E], B]): F[RUN[E], Either[A, B]]    = ???
  def never[A]: F[RUN[E], A]                                                     = Transact.fail[F, A].atomically[E]
  def fireAndForget[A](fa: F[RUN[E], A]): F[RUN[E], Unit]                        = Transact.exec(fa.attempt[Void].void)

}
