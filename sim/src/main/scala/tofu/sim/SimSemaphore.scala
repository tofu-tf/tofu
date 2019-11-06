package tofu.sim
import Transact._
import cats.effect.concurrent.Semaphore
import tofu.sim.SIM.{IO, IOGuarantee, IOMonad, STMMonad, TVAR}
import tofu.syntax.bracket._
import tofu.syntax.monadic._

case class SimSemaphore[F[_, _]: IOMonad: STMMonad: Transact: IOGuarantee](tvar: F[TVAR, Long], max: Long)
    extends Semaphore[F[IO, *]] {
  def available: F[IO, Long] = tvar.read.map(max - _).atomically
  def count: F[IO, Long]     = tvar.read.atomically
  def acquireN(n: Long): F[IO, Unit] = tvar.read
    .flatMap(count => {
      if (n > max) true.pureSTM
      else if (n + count > max) fail[F, Boolean]
      else tvar.write(n + count) as false
    })
    .atomically
    .flatMap(panic[F, Unit](s"too large acquire in Semaphore $n, allowed: $max").whenA)
  def tryAcquireN(n: Long): F[IO, Boolean] =
    tvar.read.flatMap { count =>
      if (n > max) false.pureSTM
      else if (n + count > max) false.pureSTM
      else tvar.write(n + count) as true
    }.atomically
  def releaseN(n: Long): F[IO, Unit] =
    tvar.read.flatMap(count => tvar.write((count - n).max(0))).atomically
  def withPermit[A](t: F[IO, A]): F[IO, A] =
    (acquire *> t).guaranteeIf(_ => release)
}
