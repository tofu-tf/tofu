package tofu.sim
import cats.Id
import cats.data.{EitherT, OptionT}
import cats.effect.{CancelToken, ExitCase, Fiber}
import cats.free.Free
import tofu.sim.Sim.{Canceled, Failed, FiberExit, FiberRes, Succeed, Working, atomically, cancelMe}
import tofu.sim.SimT.{readTVar, writeTVar}
import tofu.syntax.functionK.funK

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.Random
import tofu.syntax.monadic._

final class SimTVar[A](
    private[tofu] var value: A,
    private[tofu] var assigned: Option[A] = None,
    private[tofu] val waiters: mutable.Buffer[Long] = mutable.Buffer.empty
) {
  def assign() = assigned.foreach(value = _)
}

object SimT {
  def pure[A](x: A): SimT[A]         = OptionT.pure(x)
  val unit                           = pure(())
  def fail[A]: SimT[A]               = OptionT.none
  def lift[A](fa: SimTF[A]): SimT[A] = OptionT.liftF(Free.liftF(fa))

  def readTVar[A](tvar: SimTVar[A]): SimT[A] = lift(trans => {
    trans.read += tvar
    tvar.assigned.getOrElse(tvar.value)
  })

  def writeTVar[A](tvar: SimTVar[A])(value: A): SimT[Unit] = lift(trans => {
    trans.write += tvar
    tvar.assigned = Some(value)
  })
}

object Sim {

  def pure[E, A](x: A): Sim[E, A]        = EitherT.pure(x)
  def raise[E, A](e: E): Sim[E, A]       = EitherT.leftT(e)
  def lift[E, A](fa: SimF[A]): Sim[E, A] = EitherT.liftF(Free.liftF(fa))

  def atomically[E, A](trans: SimT[A]): Sim[E, A] = lift { (runtime, fiberId) =>
    val journal = new Journal
    val result  = trans.value.foldMap[Id](funK(_(journal)))
    if (result.isDefined)
      journal.write.foreach { w =>
        w.assign()
        w.waiters.foreach(runtime.notify)
      } else journal.read.foreach(_.waiters += fiberId)

    (journal.read.iterator ++ journal.write.iterator).foreach(_.assigned = None)
    result.fold[Exit[A]](Lock)(Success(_))

  }

  def newTVar[E, A](value: A): Sim[E, SimTVar[A]]      = lift((_, _) => Success(new SimTVar[A](value)))
  def sleep[E](nanos: Long): Sim[E, Unit]              = lift((_, _) => Sleep((), nanos))
  def sleep[E](duration: FiniteDuration): Sim[E, Unit] = sleep(duration.toNanos)

  def time[E]: Sim[E, Unit] = lift((runtime, _) => Success(runtime.time))

  def exec[E](process: Sim[Nothing, Unit]): Sim[E, Unit] = lift(
    (runtime, _) => Success(runtime.exec(id => process.value.map(_.merge).mapK[Exit](funK(_(runtime, id)))))
  )
  def cancel[E](fiberId: Long): Sim[E, Unit]  = lift((runtime, _) => Success(runtime.cancel(fiberId)))
  def getFiberId[E]: Sim[E, Long]             = lift((_, fiberId) => Success(fiberId))
  def panic[E, A](message: String): Sim[E, A] = lift((_, _) => Panic(message))

  sealed trait FiberRes[+E, +A]
  case object Working extends FiberRes[Nothing, Nothing]

  sealed trait FiberExit[+E, +A] extends FiberRes[E, A]

  final case class Succeed[+A](a: A) extends FiberExit[Nothing, A]
  final case class Failed[+E](e: E)  extends FiberExit[E, Nothing]
  case object Canceled               extends FiberExit[Nothing, Nothing]

  sealed trait Exit[+A]
  case object Lock                      extends Exit[Nothing]
  case class Sleep[+A](a: A, dur: Long) extends Exit[A]
  case class Success[+A](a: A)          extends Exit[A]
  case class Panic(message: String)     extends Exit[Nothing]
}

private[tofu] class Journal {
  val read: mutable.Set[SimTVar[_]]  = mutable.Set.empty
  val write: mutable.Set[SimTVar[_]] = mutable.Set.empty
}

private[tofu] class Runtime(chooser: Int => Int) {
  var nextFiberId: Long                                 = 0
  var time: Long                                        = 0
  var active: TreeMap[Long, SimFiber]                   = TreeMap.empty
  val waiting: mutable.LongMap[SimFiber]                = mutable.LongMap.empty
  val sleeping: mutable.TreeMap[Long, (Long, SimFiber)] = mutable.TreeMap.empty

  def notify(fiberId: Long): Unit =
    waiting.remove(fiberId).foreach(active += fiberId -> _)

  private def deactivate(fiberId: Long)(f: SimFiber => Unit): Unit = {
    active.get(fiberId).foreach(f)
    active -= fiberId
  }

  def sleep(fiberId: Long, dur: Long): Unit =
    deactivate(fiberId)(fib => sleeping += (time + dur) -> (fiberId -> fib))

  def sendWait(fiberId: Long): Unit =
    deactivate(fiberId)(fib => waiting += fiberId -> fib)

  def cancel(fiberId: Long): Unit = {
    active -= fiberId
    waiting -= fiberId
    sleeping -= fiberId
  }

  def exec(proc: Long => SimFiber): Unit = {
    nextFiberId += 1
    val id = nextFiberId
    active += id -> proc(id)
  }
}

object Runtime {
  def apply(chooser: Int => Int): Runtime = new Runtime(chooser)
  def apply(random: Random): Runtime      = apply(random.nextInt(_: Int))
  def apply(seed: Long): Runtime          = apply(new Random(seed))

  sealed trait Result
  case object Finished extends Result
  case object Deadlock extends Result
  case object Ready    extends Result
}

final class FiberImpl[E, A](tvar: SimTVar[FiberRes[E, A]], fiberId: Long) extends Fiber[Sim[E, *], A] {
  def cancel: Sim[E, Unit] = atomically {
    readTVar(tvar).flatMap {
      case Working => writeTVar(tvar)(Canceled)
      case _       => SimT.unit
    }
  } *> Sim.cancel(fiberId)

  def join: Sim[E, A] = atomically {
    readTVar(tvar).flatMap[FiberExit[E, A]] {
      case Working               => SimT.fail
      case exit: FiberExit[E, A] => SimT.pure(exit)
    }
  }.flatMap {
    case Succeed(a) => Sim.pure(a)
    case Failed(e)  => Sim.raise(e)
    case Canceled   => Sim.panic("joining canceled fiber")
  }
}
