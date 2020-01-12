package tofu.concurrent

import cats.Monad
import cats.effect.{Concurrent, Fiber}
import cats.effect.concurrent.{Deferred, Ref}
import cats.instances.list._
import cats.syntax.traverse._
import fs2.Stream.Compiler
import fs2.concurrent.Queue
import tofu.Start
import tofu.concurrent.Agent.WatchedAgentOnRef
import tofu.syntax.monadic._
import tofu.syntax.start._

/**
  * The analogue of [[Ref]] or [[Atom]] which allows effectful transformations
  * of shared state, both coordinated (like `modifyM`) and not (`fireUpdateM`))
  */
trait Agent[F[_], A] {

  /**
    * Returns current value immediately
    */
  def get: F[A]

  /**
    * Waits for all scheduled transformations to complete and returns resulting value.
    * Only scheduled before the method invocation transformations are considered
    */
  def await: F[A]

  /**
    * Schedules effectful update to concrete value and waits for its completion
    * @param fa should not result in error
    */
  def setM(fa: F[A]): F[A] = updateM(_ => fa)

  /**
    * Schedules effectful transformation and waits for its completion
    * @param f is a total function, which result should not produce an error
    */
  def updateM(f: A => F[A]): F[A]

  /**
    * Schedules effectful transformation, and returns immediately
    * @param f is a total function, which result should not produce an error
    */
  def fireUpdateM(f: A => F[A]): F[Unit]

  /**
    * Schedules effectful transformation and waits for its completion
    * @param f is a total function, which result should not produce an error
    */
  def modifyM[B](f: A => F[(A, B)]): F[B]

  /**
    * Terminates processing of scheduled transformations. Note that
    * after termination methods `await`, `setM`, `updateM`, `modifyM`
    * will stuck indefinitely
    */
  def terminate: F[Unit]
}

/**
  * An extended variant of [[Agent]] which allows to set watches (i.e. subscribe)
  * on updates of the shared variable. It guarantees to perform all watch actions
  * before reporting a success of shared variable modification
  */
trait WatchedAgent[F[_], A] extends Agent[F, A] {

  /**
    * Addes a watch on next change of the shared variable
    * @param action is a total function, which accepts both old and new value of
    *               the shared variable, and performs some action
    */
  def onNextUpdate(action: (A, A) => F[Unit]): F[Unit]

  /**
    * Addes a watch on each subsequent change of the shared variable
    * @param action is a total function, which accepts both old and new value of
    *               the shared variable, and performs some action
    */
  def onEachUpdate(action: (A, A) => F[Unit]): F[Unit]
}

object Agent {
  final case class Mutation[F[_]: Monad, A, B](mutation: A => F[(A, B)], promise: Deferred[F, B]) {
    def run(oldA: A, ref: Ref[F, A], watches: List[Watch[F, A]]): F[A] =
      mutation(oldA) >>= {
        case (newA, b) =>
          ref.set(newA) *> watches.traverse(w => w.onChange(oldA, newA)) *>
            promise.complete(b) as newA
      }
  }

  sealed trait Watch[F[_], A] {
    val onChange: (A, A) => F[Unit]
  }
  final case class DisposableWatch[F[_], A](onChange: (A, A) => F[Unit]) extends Watch[F, A]
  final case class ConstantWatch[F[_], A](onChange: (A, A) => F[Unit])   extends Watch[F, A]

  final case class WatchedAgentOnRef[F[_]: Monad: Deferreds, K, A](
      ref: Ref[F, A],
      mutations: Queue[F, Mutation[F, A, _]],
      watchesRef: Ref[F, List[Watch[F, A]]],
      worker: Fiber[F, Unit]
  )(implicit compiler: Compiler[F, F])
      extends WatchedAgent[F, A] {
    def get: F[A]   = ref.get
    def await: F[A] = updateM(_.pure[F])
    def updateM(f: A => F[A]): F[A] =
      modifyM(a => f(a).map(a => (a, a)))
    def fireUpdateM(f: A => F[A]): F[Unit] =
      newDeffered[F, A] >>= { promise =>
        mutations.enqueue1(Mutation[F, A, A](a => f(a).map(a => (a, a)), promise))
      }
    def modifyM[B](f: A => F[(A, B)]): F[B] =
      newDeffered[F, B] >>= { promise =>
        mutations.enqueue1(Mutation(f, promise)) *> promise.get
      }
    def onNextUpdate(action: (A, A) => F[Unit]): F[Unit] = watchesRef.update(DisposableWatch(action) :: _)
    def onEachUpdate(action: (A, A) => F[Unit]): F[Unit] = watchesRef.update(ConstantWatch(action) :: _)
    def terminate: F[Unit]                               = worker.cancel
  }
}

trait MakeAgent[F[_]] {
  def agent[A](a: A, n: Int): F[Agent[F, A]]
  def watchedAgent[A](a: A, n: Int): F[WatchedAgent[F, A]]
}

object MakeAgent {
  def apply[F[_]](implicit makeAgent: MakeAgent[F]) = new Applier[F](makeAgent)

  class Applier[F[_]](val makeAgent: MakeAgent[F]) extends AnyVal {
    def of[A](a: A, n: Int = 1000): F[Agent[F, A]]             = makeAgent.agent(a, n)
    def watched[A](a: A, n: Int = 1000): F[WatchedAgent[F, A]] = makeAgent.watchedAgent(a, n)
  }

  implicit def syncInstance[F[_]: Concurrent: Deferreds: Start]: MakeAgent[F] =
    new MakeAgent[F] {
      def agent[A](a: A, n: Int): F[Agent[F, A]] =
        watchedAgent(a, n).widen

      def watchedAgent[A](a: A, n: Int): F[WatchedAgent[F, A]] =
        for {
          ref        <- newRef.of(a)
          mutations  <- Queue.bounded[F, Agent.Mutation[F, A, _]](n)
          watchesRef <- newRef.of(List.empty[Agent.Watch[F, A]])
          worker <- (for {
                     mutation <- mutations.dequeue1
                     oldA     <- ref.get
                     watches <- watchesRef.modify(watches =>
                                 (watches.collect { case watch @ Agent.ConstantWatch(_) => watch }, watches)
                               )
                     _ <- mutation.run(oldA, ref, watches)
                   } yield ()).foreverM.void.start
        } yield WatchedAgentOnRef(ref, mutations, watchesRef, worker)
    }
}
