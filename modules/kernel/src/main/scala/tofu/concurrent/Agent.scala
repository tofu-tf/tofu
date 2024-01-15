package tofu.concurrent

import tofu.internal.instances.MakeAgentInstance

/** A mutable atomic reference augmented with effectual operations. Can be thought as TF version of zio.RefM
  */
trait Agent[F[_], A] extends SerialAgent[F, A] {

  /** Enqueue transformation, return immediately.
    *
    * @param f
    *   function to atomically modify the `Agent`
    * @return
    *   `F[Unit]`
    */
  def fireUpdateM(f: A => F[A]): F[Unit]

}

object Agent {
  type Make[F[_]] = MakeAgent[F, F]

  /** A helper for creating instances of [[tofu.concurrent.Agent]] that use same effect during construction and work. If
    * you want to use different effect to construct `Agent` use [[tofu.concurrent.MakeAgent]]
    *
    * Sample usage:
    * {{{
    * import cats.Monad
    * import cats.implicits._
    * import cats.syntax.flatMap._
    * import cats.effect.Sync
    * import tofu.concurrent.Agents
    * import tofu.common.Console
    *
    * def example[F[_]: Agents: Sync: Monad: Console]: F[Unit] =
    *       for {
    *         _ <- Monad[F].unit
    *         agent <- Agents[F].of(42)
    *         newValue <- agent.updateM(a => Console[F].putStrLn(s"current value is $a") *> Monad[F].pure(a + 27))
    *         _ <- Console[F].putStrLn(s"new value is $newValue") // new value is 69
    *       } yield ()
    * }}}
    */
  def Make[F[_]](implicit makeAgent: Make[F]): MakeAgent.Applier[F, F] = new MakeAgent.Applier[F, F](makeAgent)
}

/** A creator of [[tofu.concurrent.Agent]] that supports effectual construction.
  *
  * @tparam I
  *   effect for creation of agent
  * @tparam F
  *   effect on which agent will be run
  */
trait MakeAgent[I[_], F[_]] {

  /** Creates instance of [[tofu.concurrent.Agent]] with given value
    *
    * @param a
    *   value to be contained in `Agent`
    * @return
    *   `I[Agent[F, A]]`
    */
  def agentOf[A](a: A): I[Agent[F, A]]
}

@deprecated("Use Agent.Make", since = "0.10.4")
object Agents {
  def apply[F[_]](implicit agents: Agent.Make[F]): MakeAgent.Applier[F, F] = new MakeAgent.Applier[F, F](agents)
}

/** A helper for creating instances of [[tofu.concurrent.Agent]] that use different effects during construction and
  * work. If you want to use same effect to construct and run `Agent` use [[tofu.concurrent.Agents]]
  *
  * Sample usage:
  * {{{
  * import cats.Monad
  * import cats.implicits._
  * import cats.syntax.flatMap._
  * import cats.effect.Sync
  * import tofu.Fire
  * import tofu.concurrent.{Agents, MakeAgent, MakeRef, MakeSemaphore, Refs, Semaphores}
  * import tofu.common.Console
  *
  * def example[F[_]: Agents: Fire: Monad: Console: Sync: Refs: Semaphores](
  *     implicit
  *     refs: MakeRef[Option, F],
  *     sems: MakeSemaphore[Option, F]
  * ): F[Unit] =
  *   for {
  *     _        <- Monad[F].unit
  *     agent    <- MakeAgent[Option, F].of(42).map(Monad[F].pure(_)).getOrElse(Agents[F].of(42))
  *     newValue <- agent.updateM(a => Console[F].putStrLn(s"current value is $a") *> Monad[F].pure(a + 27))
  *     _        <- Console[F].putStrLn(s"new value is $newValue") // new value is 69
  *   } yield ()
  * }}}
  */
object MakeAgent extends MakeAgentInstance {
  def apply[I[_], F[_]](implicit mkAgent: MakeAgent[I, F]): Applier[I, F] = new Applier[I, F](mkAgent)

  /** Partially-applied creation of Agent for better type inference */
  final class Applier[I[_], F[_]](private val mkAgent: MakeAgent[I, F]) extends AnyVal {
    def of[A](a: A): I[Agent[F, A]] = mkAgent.agentOf(a)
  }
}
