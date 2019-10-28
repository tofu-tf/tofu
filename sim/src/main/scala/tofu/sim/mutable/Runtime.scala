package tofu.sim.mutable

import cats.Monad
import tofu.sim._

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.util.Random

class Runtime private[tofu] (chooser: Int => Int) {
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

  def step(): StepStatus =
    if (active.nonEmpty) activeStep()
    else if (sleeping.nonEmpty) moveSleep()
    else if (waiting.nonEmpty) Deadlock
    else Ready

  private def moveSleep(): Ready.type = Ready

  private def activeStep(): StepStatus = {
    val (idx, proc) = active.drop(chooser(active.size)).head
    proc.resume match {
      case Right(()) =>
        active -= idx
        if (idx == 0) Finished else Ready
      case Left(Success(next)) =>
        active += (idx -> next)
        Ready
      case Left(Sleep(next, dur)) =>
        active -= idx
        sleeping += (time + dur) -> (idx -> next)
        Ready
      case Left(Lock) =>
        active -= idx
        waiting += (idx -> proc)
        Ready
      case Left(p @ Panic(_)) => p
    }
  }
}
object Runtime {
  def apply(chooser: Int => Int): Runtime = new Runtime(chooser)
  def apply(random: Random): Runtime      = apply(random.nextInt(_: Int))
  def apply(seed: Long): Runtime          = apply(new Random(seed))


//  def run[E, A](sim: Sim[E, A]): Result[E, A] = {
//    val tvar = new SimTVar[E, A]()
//  }

  private val simAtomicAny: SimAtomic[Any] = new SimAtomic[Any]

  implicit def simInstance[E]: SimAtomic[E] = simAtomicAny.asInstanceOf[SimAtomic[E]]

}

object SimTransact extends Transact[SimT, SimTVar] {
  def readTVar[A](tvar: SimTVar[A]): SimT[A] = SimT.readTVar(tvar)

  def writeTVar[A](tvar: SimTVar[A], value: A): SimT[Unit] = SimT.writeTVar(tvar)(value)

  def fail[A]: SimT[A] = SimT.fail
}

class SimAtomic[E] private[sim] extends Atomic[Sim[E, *]] with Simulated[Sim[E, *]] {
  type T[A]    = SimT[A]
  type TVar[A] = SimTVar[A]
  type FiberId = Long

  implicit def transact: Transact[SimT, SimTVar] = SimTransact

  def tmonad: Monad[SimT] = implicitly

  def atomically[A](v: SimT[A]): Sim[E, A] = Sim.atomically(v)

  def newTVal[A](a: A): Sim[E, SimTVar[A]] = Sim.newTVar(a)

  def panic[A](s: String): Sim[E, A] = Sim.panic(s)

  def cancel(threadId: Long): Sim[E, Unit] = Sim.cancel(threadId)

  def fiberId: Sim[E, Long] = Sim.getFiberId
}
