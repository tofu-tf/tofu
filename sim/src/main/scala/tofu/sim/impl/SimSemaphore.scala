package tofu.sim
package impl
import Transact._
import cats.effect.concurrent.Semaphore
import tofu.sim.SIM._
import tofu.syntax.bracket._
import tofu.syntax.monadic._

case class SimSemaphore[F[+_, _]: IOMonad[*[_, _], E]: VoidMonad: STMVMonad: Transact: IOGuarantee[*[_, _], E], E](
    tvar: F[TVAR, Long],
    max: Long
) extends Semaphore[F[RUN[E], *]] {
  def available: F[RUN[E], Long]                   = tvar.read.map(max - _).atomically
  def count: F[RUN[E], Long]                       = tvar.read.atomically
  def acquireN(n: Long): F[RUN[E], Unit]           = tvar.read
    .flatMap(count => {
      if (n > max) true.pureSTM
      else if (n + count > max) fail[F, Boolean]
      else tvar.write(n + count) as false
    })
    .atomically
    .flatMap(panic[F, Nothing, Unit](s"too large acquire in Semaphore $n, allowed: $max").whenA)
  def tryAcquireN(n: Long): F[RUN[E], Boolean]     =
    tvar.read.flatMap { count =>
      if (n > max) false.pureSTM
      else if (n + count > max) false.pureSTM
      else tvar.write(n + count) as true
    }.atomically
  def releaseN(n: Long): F[RUN[E], Unit]           =
    tvar.read.flatMap(count => tvar.write((count - n).max(0))).atomically
  def withPermit[A](t: F[RUN[E], A]): F[RUN[E], A] =
    (acquire *> t).guaranteeIf(_ => release)
}
