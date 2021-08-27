package tofu.concurrent

import cats.{FlatMap, Functor, Monad}
import tofu.lift.Lift
import cats.effect.concurrent.{Ref, Semaphore}
import tofu.syntax.monadic._
import tofu.syntax.lift._

/** A less powerful version of [[tofu.concurrent.Agent]]. It does not have the `fireUpdateM` method and thus can be
  * created for greater variety of `F`
  */
trait SerialAgent[F[_], A] {

  /** Reads the value from the `SerialAgent`.
    *
    * @return
    *   `F[A]` value from `SerialAgent`
    */
  def get: F[A]

  /** Update value with effectful transformation, wait for the new result
    *
    * @param f
    *   function to atomically modify the `SerialAgent`
    * @return
    *   `F[A]` modified value of `SerialAgent`
    */
  def updateM(f: A => F[A]): F[A]

  /** Modify value with effectful transformation, calculating result.
    *
    * @param f
    *   function which computes a return value for the modification
    * @return
    *   `F[B]` modified value of `SerialAgent`
    */
  def modifyM[B](f: A => F[(B, A)]): F[B]

  /** Modifies the `SerialAgent` with the specified partial function. If function is undefined on the current value it
    * doesn't change it.
    *
    * @param f
    *   partial function to modify the `SerialAgent`
    * @return
    *   `F[A]` modified value of `SerialAgent`
    */
  def updateSomeM(f: PartialFunction[A, F[A]]): F[A]

  // NOTE: B => F[B] looks like tagless encoding of F[Option[B]]
  // we are choosing towards ZIO to simplify adoption
  /** Modifies the `SerialAgent` with the specified partial function, which computes a return value for the modification
    * if the function is defined in the current value. Otherwise it returns a default value.
    *
    * @param default
    *   value to be returned if the partial function is not defined on the current value
    * @param f
    *   partial function to modify the `SerialAgent`
    * @return
    *   `F[B]` modified value of `SerialAgent`
    */
  def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B]
}

object SerialAgent {

  /** Default implementation of [[tofu.concurrent.SerialAgent]] that consists of [[cats.effect.concurrent.Ref]] and
    * [[cats.effect.concurrent.Semaphore]]
    */
  final case class SerialSemRef[F[_]: Monad, A](ref: Ref[F, A], sem: Semaphore[F]) extends SerialAgent[F, A] {
    def get: F[A]                                                          = ref.get
    def updateM(f: A => F[A]): F[A]                                        = sem.withPermit(ref.get >>= (f(_) flatTap ref.set))
    def modifyM[B](f: A => F[(B, A)]): F[B]                                =
      sem.withPermit(ref.get >>= (f(_).flatMap { case (b, a) => ref.set(a) as b }))
    def updateSomeM(f: PartialFunction[A, F[A]]): F[A]                     =
      updateM(a => if (f.isDefinedAt(a)) f(a) else a.pure[F])
    def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B] =
      modifyM(a => if (f.isDefinedAt(a)) f(a) else (default, a).pure[F])
  }

  /** If instances of [[cats.effect.concurrent.Ref]] and [[cats.effect.concurrent.Semaphore]] can not be created for
    * some `G[_]`, but can be created for some `F[_]`, for which an instance of [[tofu.lift.Lift]] `Lift[F, G]` is
    * present, this implementation can be used
    */
  final case class UnderlyingSemRef[F[_]: Functor, G[_]: Monad: Lift[F, *[_]], A](ref: Ref[F, A], sem: Semaphore[F])
      extends SerialAgent[G, A] {
    override def get: G[A] = ref.get.lift[G]

    override def updateM(f: A => G[A]): G[A] =
      for {
        _        <- sem.acquire.lift
        oldValue <- get
        newValue <- f(oldValue)
        _        <- ref.set(oldValue).lift
        _        <- sem.release.lift
      } yield newValue

    override def modifyM[B](f: A => G[(B, A)]): G[B] =
      for {
        _        <- sem.acquire.lift
        oldValue <- get
        newValue <- f(oldValue)
        result   <- ref.set(newValue._2).as(newValue._1).lift
        _        <- sem.release.lift
      } yield result

    override def updateSomeM(f: PartialFunction[A, G[A]]): G[A] =
      updateM(a => if (f.isDefinedAt(a)) f(a) else a.pure[G])

    override def modifySomeM[B](default: B)(f: PartialFunction[A, G[(B, A)]]): G[B] =
      modifyM(a => if (f.isDefinedAt(a)) f(a) else (default, a).pure[G])
  }
}

/** A creator of [[tofu.concurrent.SerialAgent]] that supports effectful construction
  * @tparam I
  *   effect for creation of agent
  * @tparam F
  *   effect on which agent will run
  */
trait MakeSerialAgent[I[_], F[_]] {

  /** Creates instance of [[tofu.concurrent.SerialAgent]] with given value
    *
    * @param a
    *   value to be contained in `SerialAgent`
    * @return
    *   `I[SerialAgent[F, A]]`
    */
  def serialAgentOf[A](a: A): I[SerialAgent[F, A]]
}

/** A helper for creating instances of [[tofu.concurrent.SerialAgent]] that use the same effect during construction and
  * work. If you want to use different effect to construct `SerialAgent` use [[tofu.concurrent.MakeSerialAgent]]
  *
  * sample usage:
  * {{{
  * import cats.Monad
  * import cats.implicits._
  * import cats.syntax.flatmap._
  * import cats.effect.Sync
  * import tofu.concurrent.SerialAgents
  * import tofu.common.Console
  *
  * def example[F[_]: Agents: Sync: Monad: Console]: F[unit] =
  *       for {
  *         _ <- Monad[F].unit
  *         serialAgent <- SerialAgents[f].of(42)
  *         newValue <- serialAgent.updateM(a => Console[F].putStrLn(s"current value is $a") *> Monad[f].pure(a + 27))
  *         _ <- console[f].putstrln(s"new value is $newValue") // new value is 69
  *       } yield ()
  * }}}
  */
object SerialAgents {
  def apply[F[_]](implicit serialAgents: SerialAgents[F]): MakeSerialAgent.SerialApplier[F, F] =
    new MakeSerialAgent.SerialApplier[F, F](serialAgents)
}

/** A helper for creating instances of [[tofu.concurrent.SerialAgent]] that use different effects during construction
  * and work. If you want to use same effect to construct and run `SerialAgent` use [[tofu.concurrent.SerialAgents]]
  *
  * Sample usage:
  * {{{
  * import cats.Monad
  * import cats.implicits._
  * import cats.syntax.flatMap._
  * import cats.effect.Sync
  * import tofu.Fire
  * import tofu.concurrent.{SerialAgents, MakeSerialAgent, MakeRef, MakeSemaphore, Refs, Semaphores}
  * import tofu.common.Console
  *
  * def example[F[_]: SerialAgents: Monad: Console: Sync: Refs: Semaphores](
  *     implicit
  *     refs: MakeRef[Option, F],
  *     sems: MakeSemaphore[Option, F]
  * ): F[Unit] =
  *   for {
  *     _        <- Monad[F].unit
  *     serialAgent    <- MakeSerialAgent[Option, F].of(42).map(Monad[F].pure(_)).getOrElse(SerialAgents[F].of(42))
  *     newValue <- serialAgent.updateM(a => Console[F].putStrLn(s"current value is $a") *> Monad[F].pure(a + 27))
  *     _        <- Console[F].putStrLn(s"new value is $newValue") // new value is 69
  *   } yield ()
  * }}}
  */
object MakeSerialAgent extends MakeSerialAgentInstances0 {

  def apply[I[_], F[_]](implicit mkSerialAgent: MakeSerialAgent[I, F]): SerialApplier[I, F] =
    new SerialApplier[I, F](mkSerialAgent)

  final class SerialApplier[I[_], F[_]](private val mkSerialAgent: MakeSerialAgent[I, F]) extends AnyVal {
    def of[A](a: A): I[SerialAgent[F, A]] = mkSerialAgent.serialAgentOf(a)
  }

  implicit def byRefAndSemaphore[I[_]: FlatMap, F[_]: Monad](implicit
      refs: MakeRef[I, F],
      sems: MakeSemaphore[I, F]
  ): MakeSerialAgent[I, F] = new MakeSerialAgent[I, F] {
    override def serialAgentOf[A](a: A): I[SerialAgent[F, A]] =
      for {
        ref <- refs.refOf(a)
        sem <- sems.semaphore(1)
      } yield SerialAgent.SerialSemRef(ref, sem)
  }

}

private[concurrent] trait MakeSerialAgentInstances0 {
  implicit def byUnderlyingRefAndSemaphore[I[_]: FlatMap, F[_]: Functor, G[_]: Monad: Lift[F, *[_]]](implicit
      refs: MakeRef[I, F],
      sems: MakeSemaphore[I, F]
  ): MakeSerialAgent[I, G] = new MakeSerialAgent[I, G] {
    override def serialAgentOf[A](a: A): I[SerialAgent[G, A]] =
      for {
        ref <- refs.refOf(a)
        sem <- sems.semaphore(1)
      } yield SerialAgent.UnderlyingSemRef(ref, sem)
  }
}
