package tofu.zioInstances
import cats.effect.SyncIO
import cats.effect.concurrent.{Deferred, MVar, Ref, Semaphore}
import cats.~>
import tofu.concurrent.{MakeConcurrent, MakeRef}
import tofu.syntax.functionK
import tofu.syntax.functionK.funK
import tofu.zioInstances.ZioTofuConcurrentInstance.{delayTotal, rethrow}
import zio.{Promise, Queue, RIO, Task, TaskR, UIO, ZIO}
import zio.interop.catz._

class ZioTofuConcurrentInstance[R, E] extends MakeConcurrent[UIO, ZIO[R, E, *]] {
  def mvarOf[A](a: A): UIO[MVar[ZIO[R, E, *], A]] =
    delayTotal(MVar.in[SyncIO, Task, A](a).map(_.mapK(rethrow[R, E])))
  def mvarEmpty[A]: UIO[MVar[ZIO[R, E, *], A]] =
    delayTotal(MVar.emptyIn[SyncIO, Task, A].map(_.mapK(rethrow[R, E])))
  def deferred[A]: UIO[Deferred[ZIO[R, E, *], A]] =
    Promise.make[E, A].map(ZioDeferred(_))
  def semaphore(count: Long): UIO[Semaphore[ZIO[R, E, *]]] = ???
  def refOf[A](a: A): UIO[Ref[ZIO[R, E, *], A]]            = ???
}

object ZioTofuConcurrentInstance {
  private[tofu] def rethrow[R, E]: Task ~> ZIO[R, E, *]  = funK(_.catchAll(t => throw t))
  private[tofu] def delayTotal[A](fa: SyncIO[A]): UIO[A] = UIO.effectTotal(fa.unsafeRunSync())
}

case class ZioDeferred[R, E, A](p: zio.Promise[E, A]) extends Deferred[ZIO[R, E, *], A] {
  def get: ZIO[R, E, A]               = p.await
  def complete(a: A): ZIO[R, E, Unit] = p.succeed(a).unit
}

case class ZioSemapthore[R, E, A](s: zio.Semaphore, size: Long) extends Semaphore[ZIO[R, E, *]] {
  def available: ZIO[R, E, Long] = s.available
  def count: ZIO[R, E, Long] = ZIO.succeed(size)
  def acquireN(n: Long): ZIO[R, E, Unit] = s.acquireN(n)
  def tryAcquireN(n: Long): ZIO[R, E, Boolean] = s.
  def releaseN(n: Long): ZIO[R, E, Unit] = ???
  def withPermit[A](t: ZIO[R, E, A]): ZIO[R, E, A] = ???
}
