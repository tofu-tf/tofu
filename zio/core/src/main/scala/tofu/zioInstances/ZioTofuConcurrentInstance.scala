package tofu.zioInstances
import cats.data.State
import cats.effect.SyncIO
import cats.effect.concurrent.{Deferred, MVar, Ref, Semaphore}
import cats.~>
import tofu.concurrent.{Daemon, Daemonic, Exit, MakeConcurrent}
import tofu.syntax.functionK.funK
import tofu.zioInstances.ZIODaemon.exitMap
import tofu.zioInstances.ZioTofuConcurrentInstance.{delayTotal, rethrow, widen}
import zio.interop.catz._
import zio.{Cause, Promise, RIO, UIO, ZIO}

abstract class ZioTofuConcurrentInstance[R1, E1, R, E]
    extends MakeConcurrent[ZIO[R1, E1, *], ZIO[R, E, *]] with Daemonic[ZIO[R, E, *], Cause[E]]

class ZioTofuConcurrentInstanceUIO[R, E] extends ZioTofuConcurrentInstance[Any, Nothing, R, E] {
  def mvarOf[A](a: A): UIO[MVar[ZIO[R, E, *], A]] =
    delayTotal(MVar.in[SyncIO, RIO[R, *], A](a).map(_.mapK(rethrow[R, E])))
  def mvarEmpty[A]: UIO[MVar[ZIO[R, E, *], A]] =
    delayTotal(MVar.emptyIn[SyncIO, RIO[R, *], A].map(_.mapK(rethrow[R, E])))
  def deferred[A]: UIO[Deferred[ZIO[R, E, *], A]] =
    Promise.make[E, A].map(ZioDeferred(_))
  def semaphore(count: Long): UIO[Semaphore[ZIO[R, E, *]]] =
    delayTotal(Semaphore.in[SyncIO, RIO[R, *]](count).map(_.imapK(rethrow, widen)))
  def refOf[A](a: A): UIO[Ref[ZIO[R, E, *], A]] =
    zio.Ref.make(a).map(ZioRef(_))

  def daemonize[A](process: ZIO[R, E, A]): ZIO[R, E, Daemon[ZIO[R, E, *], Cause[E], A]] =
    process.fork.map(ZIODaemon(_))
}

object ZioTofuConcurrentInstance {
  private case class WrapperError[E](err: E) extends RuntimeException

  private[tofu] def rethrow[R, E]: RIO[R, *] ~> ZIO[R, E, *] = funK(_.mapError {
    case e: WrapperError[E] => e.err
    case t                  => throw t
  })

  private[tofu] def widen[R, E]: ZIO[R, E, *] ~> RIO[R, *] =
    funK(_.mapError(e => throw WrapperError(e)))
  private[tofu] def delayTotal[A](fa: SyncIO[A]): UIO[A] = UIO.effectTotal(fa.unsafeRunSync())
}

case class ZioDeferred[R, E, A](p: zio.Promise[E, A]) extends Deferred[ZIO[R, E, *], A] {
  def get: ZIO[R, E, A]               = p.await
  def complete(a: A): ZIO[R, E, Unit] = p.succeed(a).unit
}

case class ZioRef[R, E, A](r: zio.Ref[A]) extends Ref[ZIO[R, E, *], A] {
  def get: ZIO[R, E, A]             = r.get
  def set(a: A): ZIO[R, E, Unit]    = r.set(a)
  def getAndSet(a: A): ZIO[R, E, A] = r.modify(a1 => (a1, a))
  def access: ZIO[R, E, (A, A => ZIO[R, E, Boolean])] =
    r.get.map { a =>
      def upd(a1: A) = r.modify { a2 =>
        val same = a.asInstanceOf[AnyRef] eq a2.asInstanceOf[AnyRef]
        if (same) (true, a1) else (false, a2)
      }
      (a, upd)
    }
  def tryUpdate(f: A => A): ZIO[R, E, Boolean]                    = r.update(f).as(true)
  def tryModify[B](f: A => (A, B)): ZIO[R, E, Option[B]]          = r.modify(f(_).swap).map(Some(_))
  def update(f: A => A): ZIO[R, E, Unit]                          = r.update(f).unit
  def modify[B](f: A => (A, B)): ZIO[R, E, B]                     = r.modify(f(_).swap)
  def tryModifyState[B](state: State[A, B]): ZIO[R, E, Option[B]] = modifyState(state).map(Some(_))
  def modifyState[B](state: State[A, B]): ZIO[R, E, B]            = r.modify(a => state.run(a).value.swap)
}

case class ZIODaemon[R, E, A](fib: zio.Fiber[E, A]) extends Daemon[ZIO[R, E, *], Cause[E], A] {
  def join: ZIO[R, E, A]                         = fib.join
  def cancel: ZIO[R, E, Unit]                    = fib.interrupt.unit
  def poll: ZIO[R, E, Option[Exit[Cause[E], A]]] = fib.poll.map(_.map(exitMap))
  def exit: ZIO[R, E, Exit[Cause[E], A]]         = fib.await.map(exitMap)
}

object ZIODaemon {
  private[ZIODaemon] def exitMap[E, A](e: zio.Exit[E, A]): Exit[Cause[E], A] =
    e match {
      case zio.Exit.Success(a) => Exit.Completed(a)
      case zio.Exit.Failure(e) => if (e.interrupted) Exit.Canceled else Exit.Error(e)
    }
}
