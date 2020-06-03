package tofu
package sim
package mutable

import cats.free.Free

import scala.collection.mutable
import cats.data.EitherT

final class MutTVar[A](
    private[tofu] var value: A,
    private[tofu] var assigned: Option[A] = None,
    private[tofu] val waiters: mutable.Buffer[Long] = mutable.Buffer.empty
) {
  def assign() = assigned.foreach(value = _)
}

private[tofu] class Journal {
  val read: mutable.Set[MutTVar[_]]  = mutable.Set.empty
  val write: mutable.Set[MutTVar[_]] = mutable.Set.empty
}

object SimT {
  def pure[A](x: A): SimT[Void, A]         = EitherT.pure(x)
  val unit                                 = pure(())
  def fail[E, A]: SimT[E, A]               = EitherT.leftT[SimTFree, A](None)
  def raise[E, A](err: E): SimT[E, A]      = EitherT.leftT[SimTFree, A](Some(err))
  def lift[E, A](fa: SimTF[A]): SimT[E, A] = EitherT.liftF(Free.liftF(fa))

  def readTVar[A](tvar: MutTVar[A]): SimT[Nothing, A] = lift(trans => {
    trans.read += tvar
    tvar.assigned.getOrElse(tvar.value)
  })

  def writeTVar[A](tvar: MutTVar[A])(value: A): SimT[Nothing, Unit] = lift(trans => {
    trans.write += tvar
    tvar.assigned = Some(value)
  })
}
