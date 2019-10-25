package tofu.sim
import cats.data.OptionT
import cats.free.Free

import scala.collection.mutable

final class SimTVar[A](
  private[tofu] var value: A,
  private[tofu] var assigned: Option[A] = None,
  private[tofu] val waiters: mutable.Buffer[Long] = mutable.Buffer.empty
) {
  def assign() = assigned.foreach(value = _)
}

private[tofu] class Journal {
  val read: mutable.Set[SimTVar[_]]  = mutable.Set.empty
  val write: mutable.Set[SimTVar[_]] = mutable.Set.empty
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