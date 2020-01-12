package tofu.concurrent
import cats.Monad
import cats.effect.Concurrent
import cats.effect.concurrent.{Deferred, Ref}
import cats.instances.list._
import cats.syntax.traverse._
import fs2.concurrent.InspectableQueue
import fs2.Stream.Compiler
import tofu.Start
import tofu.concurrent.Agent.WatchedAgentOnRef
import tofu.syntax.monadic._
import tofu.syntax.start._

trait Agent[F[_], A] {
  def get: F[A]
  def await: F[A]
  def setM(fa: F[A]): F[A] = updateM(_ => fa)
  def updateM(f: A => F[A]): F[A]
  def fireUpdateM(f: A => F[A]): F[Unit]
  def modifyM[B](f: A => F[(A, B)]): F[B]
}

trait WatchedAgent[F[_], A] extends Agent[F, A] {
  def onNextUpdate(action: (A, A) => F[Unit]): F[Unit]
  def onEachUpdate(action: (A, A) => F[Unit]): F[Unit]
}

object Agent {
  final case class Mutation[F[_]: Monad, A, B](mutation: A => F[(A, B)], promise: Deferred[F, B]) {
    def run(a: A, ref: Ref[F, A]): F[A] =
      mutation(a) >>= {
        case (a, b) => ref.set(a) *> promise.complete(b) as a
      }
  }

  sealed trait Watch[F[_], A] {
    val onChange: (A, A) => F[Unit]
  }
  final case class DisposableWatch[F[_], A](onChange: (A, A) => F[Unit]) extends Watch[F, A]
  final case class ConstantWatch[F[_], A](onChange: (A, A) => F[Unit])   extends Watch[F, A]

  final case class WatchedAgentOnRef[F[_]: Monad: Deferreds, K, A](
      ref: Ref[F, A],
      mutations: InspectableQueue[F, Mutation[F, A, _]],
      watchesRef: Ref[F, List[Watch[F, A]]]
  )(implicit compiler: Compiler[F, F])
      extends WatchedAgent[F, A] {
    def get: F[A] = ref.get
    def await: F[A] =
      mutations.size.map {
        case 0 => None
        case x => Some(x)
      }.unNoneTerminate.compile.drain *> ref.get
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
  }
}

trait MakeAgent[F[_]] {
  def agent[A](a: A, n: Int = 1000): F[Agent[F, A]]
  def watchedAgent[A](a: A, n: Int = 1000): F[WatchedAgent[F, A]]
}

object MakeAgent {
  def apply[F[_]](implicit makeAgent: MakeAgent[F]) = new Applier[F](makeAgent)

  class Applier[F[_]](val makeAgent: MakeAgent[F]) extends AnyVal {
    def of[A](a: A): F[Agent[F, A]]             = makeAgent.agent(a)
    def watched[A](a: A): F[WatchedAgent[F, A]] = makeAgent.watchedAgent(a)
  }

  implicit def syncInstance[F[_]: Concurrent: Deferreds: Start]: MakeAgent[F] =
    new MakeAgent[F] {
      def agent[A](a: A, n: Int = 1000): F[Agent[F, A]] =
        watchedAgent(a, n).widen

      def watchedAgent[A](a: A, n: Int = 1000): F[WatchedAgent[F, A]] =
        for {
          ref        <- newRef.of(a)
          mutations  <- InspectableQueue.bounded[F, Agent.Mutation[F, A, _]](n)
          watchesRef <- newRef.of(List.empty[Agent.Watch[F, A]])
          _ <- (for {
                mutation <- mutations.dequeue1
                oldA     <- ref.get
                newA     <- mutation.run(oldA, ref)
                watches <- watchesRef.modify(watches =>
                            (watches.collect { case watch @ Agent.ConstantWatch(_) => watch }, watches)
                          )
                _ <- watches.traverse(w => w.onChange(oldA, newA).start)
              } yield ()).foreverM.void.start
        } yield WatchedAgentOnRef(ref, mutations, watchesRef)
    }
}
