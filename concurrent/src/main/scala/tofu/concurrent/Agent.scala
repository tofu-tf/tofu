package tofu.concurrent
import cats.effect.concurrent.{Ref, Semaphore}
import cats.{FlatMap, Monad}
import tofu.Fire
import tofu.syntax.fire._
import tofu.syntax.monadic._

trait Agent[F[_], A] {

  /** return current value, this will never block */
  def get: F[A]

  /** update value with effectful transformation, wait for the new result */
  def updateM(f: A => F[A]): F[A]

  /** enqueue transformation, return immediately */
  def fireUpdateM(f: A => F[A]): F[Unit]

  /** modify value with effectful transformation, calculating result */
  def modifyM[B](f: A => F[(B, A)]): F[B]

  /** maybe update value with effectful transformation, wait for the new result */
  def updateSomeM(f: PartialFunction[A, F[A]]): F[A]

  // B => F[B] looks like tagless encoding of F[Option[B]]
  // we are choosing towards ZIO to simplify adoption
  /** maybe modify value with effectful transformation, calculating result */
  def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B]
}

object Agent {
//  private[this] val representableAny: RepresentableK[Agent[*[_], Any]] = derived.genRepresentableK[Agent[*[_], Any]]

  final case class SemRef[F[_]: Monad: Fire, A](ref: Ref[F, A], sem: Semaphore[F]) extends Agent[F, A] {
    def get: F[A]                          = ref.get
    def updateM(f: A => F[A]): F[A]        = sem.withPermit(ref.get >>= (f(_) flatTap ref.set))
    def fireUpdateM(f: A => F[A]): F[Unit] = updateM(f).fireAndForget
    def modifyM[B](f: A => F[(B, A)]): F[B] =
      sem.withPermit(ref.get >>= (f(_).flatMap { case (b, a) => ref.set(a) as b }))
    def updateSomeM(f: PartialFunction[A, F[A]]): F[A] =
      updateM(a => if (f.isDefinedAt(a)) f(a) else a.pure[F])
    def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B] =
      modifyM(a => if (f.isDefinedAt(a)) f(a) else (default, a).pure[F])
  }
}

trait MakeAgent[I[_], F[_]] {
  def agentOf[A](a: A): I[Agent[F, A]]
}

object Agents {
  def apply[F[_]](implicit agents: Agents[F]): MakeAgent.Applier[F, F] = new MakeAgent.Applier[F, F](agents)
}

object MakeAgent {
  def apply[I[_], F[_]](implicit mkAgent: MakeAgent[I, F]): Applier[I, F] = new Applier[I, F](mkAgent)

  final class Applier[I[_], F[_]](private val mkAgent: MakeAgent[I, F]) extends AnyVal {
    def of[A](a: A): I[Agent[F, A]] = mkAgent.agentOf(a)
  }

  implicit def byRefAndSemaphore[I[_]: FlatMap, F[_]: Monad: Fire](
      implicit
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
