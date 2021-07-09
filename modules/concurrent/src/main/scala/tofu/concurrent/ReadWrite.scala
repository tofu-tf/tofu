package tofu.concurrent

import cats.ApplicativeError
import cats.effect._
import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.syntax.bracket._
import cats.syntax.option._
import tofu.syntax.monadic._

import scala.annotation.nowarn
import scala.collection.immutable.Queue

/** A purely functional ReadWriteLock.
  *
  * It declare that only a single writer at a time can modify the data,
  * and predefined count of readers can concurrently read the data.
  *
  * In other cases, it will be semantically blocked until operation become available.
  * All those wishing to access the data are serviced in order.
  */
trait ReadWrite[F[_], A] {

  /** Acquires a single reader if there is no writers in queue
    * and if the max number of readers is not reached.
    *
    * Otherwise, it will be semantically blocked until operation become available.
    *
    * WARN! Ignoring release action lead to resource leak. In most cases, `get` is preferable.
    *
    * @return current value and release action.
    */
  def read: F[(A, F[Unit])]

  /** Acquires a single writer if there is no writers in queue and no one is reading.
    *
    * Otherwise, it will be semantically blocked until operation become available.
    *
    * WARN! Ignoring putAndRelease action lead to resource leak. In most cases, `update` or `set` is preferable.
    *
    * @return current value and putAndRelease action.
    */
  def write: F[(A, A => F[Unit])]

  /** Like `read`, but does not require explicit release.
    *
    * @return value snapshot
    */
  def get: F[A]

  /** Like `tryRead`, but does not require explicit release
    *
    * @return value snapshot wrapped in `Some` or `None` if reading is unavailable.
    */
  def tryGet: F[Option[A]]

  /** Acquires a single reader if there is no writers in queue
    * and if the max number of readers is not reached.
    *
    * If not, None will be returned as a result.
    *
    * @return current value and release action wrapped in `Some` or `None` if reading is unavailable.
    */
  def tryRead: F[Option[(A, F[Unit])]]

  /** Acquires a single writer if there is no writers in queue and no one is reading.
    *
    * Or else, `None` will be returned as a result.
    *
    * @return current value and putAndRelease action wrapped in `Some` or `None` if writing is unavailable.
    */
  def tryWrite: F[Option[(A, A => F[Unit])]]

  /** Sets the current value to `a`.
    *
    * In scenarios where write operation is unavailable it may semantically blocks until operation successfully made.
    *
    * @return a task that update state value to `a` on evaluation.
    */
  def set(a: A): F[Unit] = update(_ => a)

  /** Sets the current value to `a`.
    *
    * In scenarios where write operation is unavailable it may semantically blocks until operation successfully made.
    *
    * @return whether or not the update succeeded
    */
  def trySet(a: A): F[Boolean] = tryUpdate(_ => a)

  /** Modifies the current value using the supplied update function.
    *
    * In scenarios where write operation is unavailable it may semantically blocks until operation successfully made.
    *
    * @return a task that update state value on evaluation.
    */
  def update(f: A => A): F[Unit]

  /** Attempts to modify the current value with supplied function, returning `false` if writing is unavailable.
    *
    * @return whether or not the update succeeded
    */
  def tryUpdate(f: A => A): F[Boolean]

  /** Like `tryModify` but does not complete until the update has been successfully made.
    */
  def modify[B](f: A => (A, B)): F[B]

  /** Like `tryUpdate` but allows the update function to return an output value of
    * type `B`. The returned action completes with `None` if the value is not updated
    * successfully and `Some(b)` otherwise.
    */
  def tryModify[B](f: A => (A, B)): F[Option[B]]
}

object ReadWrite {

  sealed trait Op[F[_], A] {
    val isReader: Boolean = false
    val isWriter: Boolean = false
  }
  final case class Reader[F[_], A](d: Deferred[F, (A, F[Unit])]) extends Op[F, A] {
    override val isReader: Boolean = true
  }
  final case class Writer[F[_], A](d: Deferred[F, (A, A => F[Unit])]) extends Op[F, A] {
    override val isWriter: Boolean = true
  }

  /*
   * Queue accumulates wishers operations when they cannot be served.
   * It's using tombstones on canceled operations that in queue.
   * Those operations will be ignored when release actions choose next one op.
   */
  final case class State[F[_], A](
      q: Queue[Op[F, A]],
      currentReaders: Int,
      writeLocked: Boolean,
      trashedReaders: Set[Deferred[F, (A, F[Unit])]],
      trashedWriters: Set[Deferred[F, (A, A => F[Unit])]],
      a: A
  )

  final class ConcurrentReadWrite[F[_], A](state: Ref[F, State[F, A]], maxReaders: Int)(implicit F: Concurrent[F])
      extends ReadWrite[F, A] {

    def getState: F[State[F, A]] = state.get

    override def read: F[(A, F[Unit])] =
      Deferred[F, (A, F[Unit])].bracketCase { d =>
        state.modify {
          case s @ State(q, r, false, _, _, a) if r < maxReaders && q.headOption.forall(!_.isWriter) =>
            s.copy(currentReaders = r + 1) -> d.complete(a -> releaseRead)
          case s @ State(q, _, _, _, _, _)                                                           => s.copy(q = q :+ Reader(d)) -> F.unit
        }.flatten *> d.get
      } {
        case (d, ExitCase.Canceled | ExitCase.Error(_)) =>
          state.update(s => s.copy(trashedReaders = s.trashedReaders + d))
        case _                                          => F.unit
      }

    override def write: F[(A, A => F[Unit])] =
      Deferred[F, (A, A => F[Unit])].bracketCase { d =>
        state.modify {
          case s @ State(q, 0, false, _, _, a) if q.isEmpty =>
            s.copy(writeLocked = true) -> d.complete(a -> releaseWrite)
          case s @ State(q, _, _, _, _, _)                  => s.copy(q = q :+ Writer(d)) -> F.unit
        }.flatten *> d.get
      } {
        case (d, ExitCase.Canceled | ExitCase.Error(_)) =>
          state.update(s => s.copy(trashedWriters = s.trashedWriters + d))
        case _                                          => F.unit
      }

    private def dropPrecedingTrashed(state: State[F, A]): State[F, A] =
      state.q match {
        case Reader(d) +: tl if state.trashedReaders(d) =>
          dropPrecedingTrashed(state.copy(q = tl, trashedReaders = state.trashedReaders - d))
        case Writer(d) +: tl if state.trashedWriters(d) =>
          dropPrecedingTrashed(state.copy(q = tl, trashedWriters = state.trashedWriters - d))
        case _                                          => state
      }

    private def extractReadersBatch(state: State[F, A], quantity: Int): (State[F, A], List[Reader[F, A]], Int) = {
      @nowarn("cat=other-match-analysis")
      def impl(
          state: State[F, A],
          batch: List[Reader[F, A]],
          remain: Int,
          size: Int
      ): (State[F, A], List[Reader[F, A]], Int) = {
        if (remain <= 0) (state, batch, size)
        else
          dropPrecedingTrashed(state) match {
            case s if s.q.isEmpty || s.q.head.isWriter           => (state, batch, size)
            case s @ State((r @ Reader(_)) +: tl, _, _, _, _, _) =>
              impl(s.copy(q = tl), r :: batch, remain - 1, size + 1)
          }
      }
      impl(state, Nil, quantity, 0)
    }

    @nowarn("cat=other-match-analysis")
    private val releaseRead: F[Unit] =
      state.modify {
        dropPrecedingTrashed(_) match {
          case state @ State(q, r, _, _, _, _) if q.isEmpty  => state.copy(currentReaders = r - 1) -> F.unit
          case state @ State(Reader(d) +: tl, _, _, _, _, a) => state.copy(q = tl)                 -> d.complete(a -> releaseRead)
          case state @ State(Writer(d) +: tl, 1, _, _, _, a) =>
            state.copy(q = tl, currentReaders = 0, writeLocked = true) -> d.complete(a -> releaseWrite)
          case state @ State(Writer(_) +: _, r, _, _, _, _)  => state.copy(currentReaders = r - 1) -> F.unit
        }
      }.flatten

    private def releaseWrite(newA: A): F[Unit] =
      state.modify {
        dropPrecedingTrashed(_) match {
          case state @ State(q, _, _, _, _, _) if q.isEmpty  => state.copy(a = newA, writeLocked = false) -> F.unit
          case state @ State(Writer(d) +: tl, _, _, _, _, _) =>
            state.copy(q = tl, a = newA) -> d.complete(newA -> releaseWrite)
          case s                                             =>
            val (cleanState, readersBatch, batchSize) = extractReadersBatch(s, maxReaders)
            val readers                               = readersBatch.map(_.d.complete(newA -> releaseRead))
            cleanState.copy(a = newA, writeLocked = false, currentReaders = batchSize) -> readers.foldRight(F.unit)(
              _ *> _
            )
        }
      }.flatten

    override def get: F[A] = read.flatMap { case (a, release) => release.as(a) }

    override def tryGet: F[Option[A]] =
      tryRead.flatMap {
        case Some((a, release)) => release.as(a.some)
        case _                  => F.pure(none)
      }

    override def tryRead: F[Option[(A, F[Unit])]] =
      state.modify {
        case s @ State(q, r, false, _, _, a) if r < maxReaders && q.headOption.forall(!_.isWriter) =>
          s.copy(currentReaders = r + 1) -> (a -> releaseRead).some
        case s                                                                                     => s -> none
      }

    override def tryWrite: F[Option[(A, A => F[Unit])]] =
      state.modify {
        case s @ State(q, 0, false, _, _, a) if q.isEmpty => s.copy(writeLocked = true) -> (a -> releaseWrite _).some
        case s                                            => s                          -> none
      }

    override def update(f: A => A): F[Unit] = modify(a => (f(a), ()))

    override def tryUpdate(f: A => A): F[Boolean] = tryModify(a => (f(a), ())).map(_.isDefined)

    override def modify[B](f: A => (A, B)): F[B] =
      write.flatMap { case (a, putAndRelease) =>
        val (u, b) = f(a)
        putAndRelease(u).as(b)
      }

    override def tryModify[B](f: A => (A, B)): F[Option[B]] =
      tryWrite.flatMap {
        case Some((a, putAndRelease)) =>
          val (u, b) = f(a)
          putAndRelease(u).as(b.some)
        case _                        => F.pure(none)
      }

  }

  private def assertNonNegative[F[_]](n: Int)(implicit F: ApplicativeError[F, Throwable]): F[Unit] =
    if (n < 0) F.raiseError(new IllegalArgumentException(s"n must be non-negative, was: $n")) else F.unit

  /** Creates a `ReadWrite` initialized to the supplied value.
    *
    * {{{
    *   import cats.effect.IO
    *   import tofu.concurrent.ReadWrite
    *
    *   for {
    *     rw  <- ReadWrite.of[IO, Int](10)
    *     ten <- rw.get
    *   } yield ten
    * }}}
    */
  def of[F[_]: Concurrent, A](initial: A, maxReaders: Int = Int.MaxValue): F[ReadWrite[F, A]] =
    assertNonNegative[F](maxReaders) *>
      Ref.of[F, State[F, A]](State(Queue.empty, 0, writeLocked = false, Set(), Set(), initial)).map {
        new ConcurrentReadWrite[F, A](_, maxReaders)
      }

  /**  Builds a `ReadWrite` value for data types that are [[cats.effect.Sync]]
    *  Like [[of]] but initializes state using another effect constructor
    */
  def in[F[_]: Sync, G[_]: Concurrent, A](initial: A, maxReaders: Int = Int.MaxValue): F[ReadWrite[G, A]] =
    assertNonNegative[F](maxReaders) *>
      Ref.in[F, G, State[G, A]](State(Queue.empty, 0, writeLocked = false, Set(), Set(), initial)).map {
        new ConcurrentReadWrite(_, maxReaders)
      }
}
