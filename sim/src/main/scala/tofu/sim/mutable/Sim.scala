package tofu.sim.mutable

import cats.Id
import cats.data.EitherT
import cats.effect.Fiber
import cats.free.Free
import tofu.Void
import tofu.sim._
import tofu.syntax.functionK.funK

import scala.concurrent.duration.FiniteDuration

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

  def time[E]: Sim[E, Long] = lift((runtime, _) => Success(runtime.time))

  def exec[E](process: Sim[Void, Unit]): Sim[E, Unit] = lift(
    (runtime, _) => Success(runtime.exec(id => process.value.map(Void.mergeEither).mapK[Exit](funK(_(runtime, id)))))
  )
  def cancel[E](fiberId: Long): Sim[E, Unit]  = lift((runtime, _) => Success(runtime.cancel(fiberId)))
  def getFiberId[E]: Sim[E, Long]             = lift((_, fiberId) => Success(fiberId))
  def panic[E, A](message: String): Sim[E, A] = lift((_, _) => Panic(message))

  def respondTo[E, A](proc: Sim[E, A], tvar: SimTVar[FiberRes[E, A]]): Sim[Void, Unit] =
    EitherT(proc.value.flatMap(res => Sim.atomically(SimT.writeTVar(tvar)(FiberRes.fromEither(res))).value))

  def fork[E, A](proc: Sim[E, A]): Sim[E, Fiber[Sim[E, *], A]] = {
    for {
      tvar <- newTVar[E, FiberRes[E, A]](FiberRes.Working)
      id   <- getFiberId[E]
      _    <- exec(respondTo(proc, tvar))
    } yield SimFiber[Sim[E, *], E].of[A](tvar, id)

  }

}
