package tofu.sim
import cats.Functor

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

  def step(): Result =
    if (active.nonEmpty) activeStep()
    else if (sleeping.nonEmpty) moveSleep()
    else if (waiting.nonEmpty) Deadlock
    else Ready

  private def moveSleep(): Ready.type = Ready

  private def activeStep(): Result = {
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
        sleeping += (time + dur) -> (idx, next)
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

}


