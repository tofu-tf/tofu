package tofu.sim.mutable

import cats.Id
import cats.data.EitherT
import cats.effect.Fiber
import cats.free.Free
import tofu.Void
import tofu.sim.SIM.IO
import tofu.sim._
import tofu.sim.mutable.MutSim.MutVar
import tofu.syntax.functionK.funK
import tofu.syntax.monadic._

import scala.concurrent.duration.FiniteDuration

object SimIO {

  def pure[E, A](x: A): SimIO[E, A]        = EitherT.pure(x)
  def raise[E, A](e: E): SimIO[E, A]       = EitherT.leftT(e)
  def lift[E, A](fa: SimF[A]): SimIO[E, A] = EitherT.liftF(Free.liftF(fa))

  def atomically[E, A](trans: SimT[A]): SimIO[E, A] = lift { (runtime, fiberId) =>
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

  def newTVar[E, A](value: A): SimIO[E, MutTVar[A]]      = lift((_, _) => Success(new MutTVar[A](value)))
  def sleep[E](nanos: Long): SimIO[E, Unit]              = lift((_, _) => Sleep((), nanos))
  def sleep[E](duration: FiniteDuration): SimIO[E, Unit] = sleep(duration.toNanos)

  def time[E]: SimIO[E, Long] = lift((runtime, _) => Success(runtime.time))

  def exec[E](process: SimIO[Void, Unit]): SimIO[E, Unit] = lift(
    (runtime, _) => Success(runtime.exec(id => process.value.map(Void.mergeEither).mapK[Exit](funK(_(runtime, id)))))
  )
  def cancel[E](fiberId: Long): SimIO[E, Unit]  = lift((runtime, _) => Success(runtime.cancel(fiberId)))
  def getFiberId[E]: SimIO[E, Long]             = lift((_, fiberId) => Success(fiberId))
  def panic[E, A](message: String): SimIO[E, A] = lift((_, _) => Panic(message))

  def respondTo[E, A](proc: SimIO[E, A], tvar: MutTVar[FiberRes[E, A]]): SimIO[Void, Unit] =
    EitherT(proc.value.flatMap(res => SimIO.atomically(SimT.writeTVar(tvar)(FiberRes.fromEither(res))).value))

  def fork[E, A](proc: MutSim[E, IO, A]): SimIO[E, Fiber[MutSim[E, IO, *], A]] = {
    for {
      tvar <- newTVar[E, FiberRes[E, A]](FiberRes.Working)
      id   <- getFiberId[E]
      _    <- exec(respondTo(proc.value, tvar))
    } yield SimFiber[MutSim[E, *, *], E, A](MutVar[E, FiberRes[E, A]](tvar), id)
  }

  def guarantee[E, A, B, C](
      init: SimIO[E, A]
  )(action: A => SimIO[E, B])(release: (A, Boolean) => SimIO[E, C]): SimIO[E, B] =
    init.flatMapF(a => action(a).value.flatMap(res => release(a, res.isRight).value as res))
}
