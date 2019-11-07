package tofu.sim.mutable

import tofu.sim._

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.util.Random

final class FiberInfo private[tofu] {
  val successBrackets: mutable.Stack[SimProc] = mutable.Stack.empty
  val cancelBrackets: mutable.Stack[SimProc]  = mutable.Stack.empty
  def brackets(success: Boolean): mutable.Stack[SimProc] =
    if (success) successBrackets else cancelBrackets
  var uncancelable: Boolean = false
  var inBracket: Boolean    = false
  var bracketType: Boolean  = false
  var wakeUp: Long          = -1

  def canCancel = !uncancelable && !inBracket
}

class Runtime private[tofu] (chooser: Int => Int) {
  var nextFiberId: Long                                = 0
  var time: Long                                       = 0
  var active: TreeMap[Long, SimProc]                   = TreeMap.empty
  val infos: mutable.LongMap[FiberInfo]                = mutable.LongMap.empty
  val waiting: mutable.LongMap[SimProc]                = mutable.LongMap.empty
  val sleeping: mutable.TreeMap[(Long, Long), SimProc] = mutable.TreeMap.empty
  val tracing: mutable.LongMap[mutable.Buffer[String]] = mutable.LongMap.empty

  def trace(fiberId: Long, message: String): Unit =
    tracing.getOrElseUpdate(fiberId, mutable.Buffer.empty) += message

  def notify(fiberId: Long): Unit =
    waiting.remove(fiberId).foreach(active += fiberId -> _)

  def regBracket(fiberId: FiberId, success: Boolean, proc: SimProc): Unit =
    infos(fiberId).brackets(success).push(proc)

  private def startBracketJob(fiberId: FiberId, success: Boolean) = {
    val info = infos(fiberId)

    val s = if (info.inBracket) info.bracketType else success
    info.brackets(!s).clear()
    if (info.brackets(s).nonEmpty) {
      val handler = info.brackets(s).pop()
      info.inBracket = true
      info.inBracket = s
      info.uncancelable = true
      active += fiberId -> handler
    } else infos -= fiberId
  }

  def setUncancelable(fiberId: FiberId, uncancelable: Boolean): Unit =
    infos(fiberId).uncancelable = uncancelable

  def cancel(fiberId: Long): Unit = {
    val info = infos(fiberId)
    if (info.canCancel) {
      active -= fiberId
      waiting -= fiberId
      sleeping -= (info.wakeUp -> fiberId)
      startBracketJob(fiberId, success = false)
    }
  }

  def exec(proc: Long => SimProc): Long = {
    nextFiberId += 1
    val id = nextFiberId
    active += id -> proc(id)
    infos(id) = new FiberInfo
    id
  }

  def step(): StepStatus =
    if (active.nonEmpty) activeStep()
    else if (sleeping.nonEmpty) moveSleep()
    else if (waiting.nonEmpty) Deadlock
    else Ready

  private def moveSleep() = {
    val ((time, _), _) = sleeping.head
    val elems          = sleeping.takeWhile(_._1._1 == time)
    elems.foreach {
      case (key @ (_, fiberId), proc) =>
        sleeping.remove(key)
        active += fiberId -> proc
        infos(fiberId).wakeUp = -1
    }
    Ready
  }

  private def handleStep(fiberId: FiberId, proc: SimProc): StepStatus =
    proc.resume match {
      case Right(()) =>
        active -= fiberId
        startBracketJob(fiberId, success = true)
        if (fiberId == 0) Finished else Ready
      case Left(Traced(next)) =>
        active += (fiberId -> next)
        handleStep(fiberId, next)
      case Left(Success(next)) =>
        active += (fiberId -> next)
        Ready
      case Left(Sleep(next, dur)) =>
        active -= fiberId
        val wakeUp = time + dur
        sleeping += wakeUp -> fiberId -> next
        infos(fiberId).wakeUp = wakeUp
        Ready
      case Left(Lock) =>
        active -= fiberId
        waiting += (fiberId -> proc)
        Ready
      case Left(p @ Panic(_)) => p
    }

  private def activeStep(): StepStatus =
    active.drop(chooser(active.size)).head match {
      case (fiberId, proc) => handleStep(fiberId, proc)
    }
}
object Runtime {
  def apply(chooser: Int => Int): Runtime = new Runtime(chooser)
  def apply(random: Random): Runtime      = apply(random.nextInt(_: Int))
  def apply(seed: Long): Runtime          = apply(new Random(seed))
}
