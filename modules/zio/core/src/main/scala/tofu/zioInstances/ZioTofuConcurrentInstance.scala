package tofu.zioInstances
import cats.effect.concurrent.Deferred
import tofu.concurrent._
import tofu.concurrent.impl.QVarSM
import tofu.zioInstances.ZIODaemon.exitMap
import zio.{Exit => _, _}

abstract class ZioTofuConcurrentInstance[R1, E1, R, E]
    extends MakeConcurrent[ZIO[R1, E1, *], ZIO[R, E, *]] with Daemonic[ZIO[R, E, *], Cause[E]]

class ZioTofuConcurrentInstanceUIO[R, E] extends ZioTofuConcurrentInstance[Any, Nothing, R, E] {
  def deferred[A]: UIO[Deferred[ZIO[R, E, *], A]] =
    Promise.make[E, A].map(ZioDeferred(_))

  private def makeQVar[A](opt: Option[A]): ZIO[Any, Nothing, QVar[ZIO[R, E, *], A]] =
    for (r <- Ref.make[QVarSM.State[A, zio.Promise[Nothing, A]]](QVarSM.fromOption(opt))) yield ZioQVar(r)
  def qvarOf[A](a: A): ZIO[Any, Nothing, QVar[ZIO[R, E, *], A]]                     = makeQVar(Some(a))

  def qvarEmpty[A]: ZIO[Any, Nothing, QVar[ZIO[R, E, *], A]] = makeQVar(None)

  def gatekeeper(available: Long): ZIO[Any, Nothing, Gatekeeper[ZIO[R, E, *], Long]] =
    Semaphore.make(available).map(ZioGatekeeper(_, available))

  def atom[A](a: A): ZIO[Any, Nothing, Atom[ZIO[R, E, *], A]] = Ref.make(a).map(ZioAtom(_))

  def agentOf[A](a: A): ZIO[Any, Nothing, Agent[ZIO[R, E, *], A]] = RefM.make(a).map(ZioAgent(_))

  def daemonize[A](process: ZIO[R, E, A]): ZIO[R, E, Daemon[ZIO[R, E, *], Cause[E], A]] =
    process.interruptible.forkDaemon.map(ZIODaemon(_))
}

final case class ZioDeferred[R, E, A](p: zio.Promise[E, A]) extends Deferred[ZIO[R, E, *], A] {
  def get: ZIO[R, E, A]               = p.await
  def complete(a: A): ZIO[R, E, Unit] = p.succeed(a).unit
}

final case class ZioAtom[R, E, A](r: zio.Ref[A]) extends Atom[ZIO[R, E, *], A] {
  def get: ZIO[R, E, A]                       = r.get
  def set(a: A): ZIO[R, E, Unit]              = r.set(a)
  def getAndSet(a: A): ZIO[R, E, A]           = r.modify(a1 => (a1, a))
  def update(f: A => A): ZIO[R, E, Unit]      = r.update(f).unit
  def modify[B](f: A => (A, B)): ZIO[R, E, B] = r.modify(f(_).swap)
}

import zio.interop.catz.monadErrorInstance
final case class ZioQVar[R, E, A](ref: zio.Ref[QVarSM.State[A, zio.Promise[Nothing, A]]])
    extends QVarSM[ZIO[R, E, *], A, zio.Promise[Nothing, A]] {
  override protected def get: ZIO[R, E, S]                          = ref.get
  override protected def newPromise: ZIO[R, E, Promise[Nothing, A]] = Promise.make

  override protected def complete(x: A, p: Promise[Nothing, A]): ZIO[R, E, Unit] = p.complete(UIO.succeed(x)).unit

  override protected def await(p: Promise[Nothing, A]): ZIO[R, E, A]         = p.await
  override protected def modifyF[X](f: S => (S, ZIO[R, E, X])): ZIO[R, E, X] = ref.modify(f(_).swap).flatten
  override protected def set(s: S): ZIO[R, E, Unit]                          = ref.set(s)

  override protected def onCancel(fa: ZIO[R, E, A], fc: => ZIO[R, E, Unit]): ZIO[R, E, A] = fa.onInterrupt(fc.ignore)
}

final case class ZioGatekeeper[R, E](r: zio.Semaphore, size: Long) extends Gatekeeper[ZIO[R, E, *], Long] {
  def available: ZIO[R, E, Long]                                = r.available
  def taken: ZIO[R, E, Long]                                    = r.available.map(size - _)
  def withPermit[B](t: ZIO[R, E, B]): ZIO[R, E, B]              = r.withPermit(t)
  def withPermitN[B](take: Long)(t: ZIO[R, E, B]): ZIO[R, E, B] = r.withPermits(take)(t)
}

final case class ZIODaemon[R, E, A](fib: zio.Fiber[E, A]) extends Daemon[ZIO[R, E, *], Cause[E], A] {
  def join: ZIO[R, E, A]                         = fib.join
  def cancel: ZIO[R, E, Unit]                    = fib.interrupt.unit
  def poll: ZIO[R, E, Option[Exit[Cause[E], A]]] = fib.poll.map(_.map(exitMap))
  def exit: ZIO[R, E, Exit[Cause[E], A]]         = fib.await.map(exitMap)
}

final case class ZioAgent[R, E, A](refm: RefM[A]) extends Agent[ZIO[R, E, *], A] {
  def get: ZIO[R, E, A]                                                                  = refm.get
  def updateM(f: A => ZIO[R, E, A]): ZIO[R, E, A]                                        = refm.getAndUpdate(f)
  def fireUpdateM(f: A => ZIO[R, E, A]): ZIO[R, E, Unit]                                 = refm.update(f).forkDaemon.unit
  def modifyM[B](f: A => ZIO[R, E, (B, A)]): ZIO[R, E, B]                                = refm.modify(f)
  def updateSomeM(f: PartialFunction[A, ZIO[R, E, A]]): ZIO[R, E, A]                     = refm.getAndUpdateSome(f)
  def modifySomeM[B](default: B)(f: PartialFunction[A, ZIO[R, E, (B, A)]]): ZIO[R, E, B] = refm.modifySome(default)(f)
}

object ZIODaemon {
  private[ZIODaemon] def exitMap[E, A](e: zio.Exit[E, A]): Exit[Cause[E], A] =
    e match {
      case zio.Exit.Success(a) => Exit.Completed(a)
      case zio.Exit.Failure(e) => if (e.interrupted) Exit.Canceled else Exit.Error(e)
    }
}
