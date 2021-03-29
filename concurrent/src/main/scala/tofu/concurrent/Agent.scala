package tofu.concurrent
import cats.{FlatMap, Monad}
import tofu.Fire
import tofu.syntax.fire._
import tofu.syntax.monadic._
import cats.effect.Ref
import cats.effect.std.Semaphore

/** A mutable atomic reference augmented with effectful operations.
  * Can be thought as TF version of zio.RefM
  */
trait Agent[F[_], A] extends SerialAgent[F, A] {

  /** Enqueue transformation, return immediately.
    *
    * @param f function to atomically modify the `Agent`
    * @return `F[Unit]`
    */
  def fireUpdateM(f: A => F[A]): F[Unit]

}

object Agent {
//  private[this] val representableAny: RepresentableK[Agent[*[_], Any]] = derived.genRepresentableK[Agent[*[_], Any]]

  /** Default implementation of [[tofu.concurrent.Agent]]
    * that consists of [[cats.effect.concurrent.Ref]] and [[cats.effect.concurrent.Semaphore]]
    */
  final case class SemRef[F[_]: Monad: Fire, A](ref: Ref[F, A], sem: Semaphore[F]) extends Agent[F, A] {
    def get: F[A]                                                          = ref.get
    def updateM(f: A => F[A]): F[A]                                        = sem.withPermit(ref.get >>= (f(_) flatTap ref.set))
    def fireUpdateM(f: A => F[A]): F[Unit]                                 = updateM(f).fireAndForget
    def modifyM[B](f: A => F[(B, A)]): F[B]                                =
      sem.withPermit(ref.get >>= (f(_).flatMap { case (b, a) => ref.set(a) as b }))
    def updateSomeM(f: PartialFunction[A, F[A]]): F[A]                     =
      updateM(a => if (f.isDefinedAt(a)) f(a) else a.pure[F])
    def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B] =
      modifyM(a => if (f.isDefinedAt(a)) f(a) else (default, a).pure[F])
  }
}

/** A creator of [[tofu.concurrent.Agent]] that supports effectful construction.
  *
  * @tparam I effect for creation of agent
  * @tparam F effect on which agent will be run
  */
trait MakeAgent[I[_], F[_]] {

  /** Creates instance of [[tofu.concurrent.Agent]] with given value
    *
    * @param a value to be contained in `Agent`
    * @return `I[Agent[F, A]]`
    */
  def agentOf[A](a: A): I[Agent[F, A]]
}

/** A helper for creating instances of [[tofu.concurrent.Agent]] that use same effect during construction and work.
  * If you want to use different effect to construct `Agent` use [[tofu.concurrent.MakeAgent]]
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
  *      for {
  *        _ <- Monad[F].unit
  *        agent <- Agents[F].of(42)
  *        newValue <- agent.updateM(a => Console[F].putStrLn(s"current value is $a") *> Monad[F].pure(a + 27))
  *        _ <- Console[F].putStrLn(s"new value is $newValue") // new value is 69
  *      } yield ()
  * }}}
  */
object Agents {
  def apply[F[_]](implicit agents: Agents[F]): MakeAgent.Applier[F, F] = new MakeAgent.Applier[F, F](agents)
}

/** A helper for creating instances of [[tofu.concurrent.Agent]] that use different effects during construction and work.
  * If you want to use same effect to construct and run `Agent` use [[tofu.concurrent.Agents]]
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
object MakeAgent {
  def apply[I[_], F[_]](implicit mkAgent: MakeAgent[I, F]): Applier[I, F] = new Applier[I, F](mkAgent)

  final class Applier[I[_], F[_]](private val mkAgent: MakeAgent[I, F]) extends AnyVal {
    def of[A](a: A): I[Agent[F, A]] = mkAgent.agentOf(a)
  }

  implicit def byRefAndSemaphore[I[_]: FlatMap, F[_]: Monad: Fire](implicit
      refs: MakeRef[I, F],
      sems: MakeSemaphore[I, F]
  ): MakeAgent[I, F] = new MakeAgent[I, F] {
    def agentOf[A](a: A): I[Agent[F, A]] =
      for {
        ref <- refs.refOf(a)
        sem <- sems.semaphore(1)
      } yield Agent.SemRef(ref, sem)
  }
}
