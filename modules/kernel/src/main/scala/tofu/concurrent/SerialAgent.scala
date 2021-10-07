package tofu.concurrent

import tofu.internal.carriers.{MkSerialAgentCE2Carrier, MkSerialAgentCE3Carrier}

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

  /** Modifies the `SerialAgent` with the specified partial function, which computes a return value for the modification
    * if the function is defined in the current value. Otherwise it returns a default value.
    *
    * @param default
    *   value to be returned if the partial function is not defined on the current value
    * @param f
    *   partial function to modify the `SerialAgent`
    * @return
    *   `F[B]` modified value of `SerialAgent`
    *
    * @note
    *   `B => F[B]` looks like tagless encoding of `F[Option[B]]` that we are choosing towards ZIO to simplify adoption
    */
  def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B]
}

object SerialAgent {
  type Make[F[_]] = MakeSerialAgent[F, F]

  /** A helper for creating instances of [[tofu.concurrent.SerialAgent]] that use the same effect during construction
    * and work. If you want to use different effect to construct `SerialAgent` use [[tofu.concurrent.MakeSerialAgent]]
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
  def Make[F[_]](implicit makeSerialAgent: Make[F]): MakeSerialAgent.SerialApplier[F, F] =
    new MakeSerialAgent.SerialApplier[F, F](makeSerialAgent)
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
@deprecated("Use SerialAgent.Make", since = "0.10.4")
object SerialAgents {
  def apply[F[_]](implicit makeSerialAgent: SerialAgent.Make[F]): MakeSerialAgent.SerialApplier[F, F] =
    new MakeSerialAgent.SerialApplier[F, F](makeSerialAgent)
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
object MakeSerialAgent extends MakeSerialAgentInstances {

  def apply[I[_], F[_]](implicit mkSerialAgent: MakeSerialAgent[I, F]): SerialApplier[I, F] =
    new SerialApplier[I, F](mkSerialAgent)

  final class SerialApplier[I[_], F[_]](private val mkSerialAgent: MakeSerialAgent[I, F]) extends AnyVal {
    def of[A](a: A): I[SerialAgent[F, A]] = mkSerialAgent.serialAgentOf(a)
  }

  final implicit def interopCE3[I[_], F[_]](implicit carrier: MkSerialAgentCE3Carrier[I, F]): MakeSerialAgent[I, F] =
    carrier

}

trait MakeSerialAgentInstances {
  final implicit def interopCE2[I[_], F[_]](implicit carrier: MkSerialAgentCE2Carrier[I, F]): MakeSerialAgent[I, F] =
    carrier

}
