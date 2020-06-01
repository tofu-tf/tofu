package tofu.concurrent
import cats.effect.concurrent.MVar
import cats.effect.{Concurrent, Sync}
import cats.Applicative
import tofu.Guarantee
import tofu.concurrent.QVar.QVarByMVar
import tofu.higherKind.{RepresentableK, derived}
import tofu.syntax.monadic._

/** a middleground between cats.concurrent.MVar and zio.Queue.bounded(1) */
trait QVar[+F[_], A] {

  /**
    * Returns `true` if the `MVar` is empty and can receive a `put`, or
    * `false` otherwise.
    *
    * Note that due to concurrent tasks, logic built in terms of `isEmpty`
    * is problematic.
    */
  def isEmpty: F[Boolean]

  /**
    * Fills the `MVar` if it is empty, or blocks (asynchronously)
    * if the `MVar` is full, until the given value is next in
    * line to be consumed on [[take]].
    *
    * This operation is atomic.
    *
    * @return a task that on evaluation will complete when the
    *         `put` operation succeeds in filling the `MVar`,
    *         with the given value being next in line to
    *         be consumed
    */
  def put(a: A): F[Unit]

  /**
    * Empties the `MVar` if full, returning the contained value,
    * or blocks (asynchronously) until a value is available.
    *
    * This operation is atomic.
    *
    * @return a task that on evaluation will be completed after
    *         a value was retrieved
    */
  def take: F[A]

  /**
    * Tries reading the current value, or blocks (asynchronously)
    * until there is a value available.
    *
    * This operation is atomic.
    *
    * @return a task that on evaluation will be completed after
    *         a value has been read
    */
  def read: F[A]
}

object QVar {
  implicit def representableK[A]: RepresentableK[QVar[*[_], A]] = derived.genRepresentableK[QVar[*[_], A]]

  final implicit class QVarOps[F[_], A](private val self: QVar[F, A]) extends AnyVal {
    def toAtom(implicit F: Applicative[F], FG: Guarantee[F]): Atom[F, A] = Atom.QAtom(self)
  }

  final case class QVarByMVar[F[_], A](mvar: MVar[F, A]) extends QVar[F, A] {
    override def isEmpty: F[Boolean] = mvar.isEmpty
    override def put(a: A): F[Unit]  = mvar.put(a)
    override def take: F[A]          = mvar.take
    override def read: F[A]          = mvar.read
  }
}

trait MakeQVar[I[_], F[_]] {
  def qvarOf[A](a: A): I[QVar[F, A]]
  def qvarEmpty[A]: I[QVar[F, A]]
}

object QVars {
  def apply[F[_]](implicit qvars: QVars[F]): MakeQVar.Applier[F, F] = new MakeQVar.Applier(qvars)
}

object MakeQVar {
  def apply[I[_], F[_]](implicit mkvar: MakeQVar[I, F]) = new Applier[I, F](mkvar)

  final class Applier[I[_], F[_]](private val makeMVar: MakeQVar[I, F]) extends AnyVal {
    def empty[A]: I[QVar[F, A]]    = makeMVar.qvarEmpty[A]
    def of[A](a: A): I[QVar[F, A]] = makeMVar.qvarOf(a)
  }

  implicit def concurrentMakeMVar[I[_]: Sync, F[_]: Concurrent]: MakeQVar[I, F] = new MakeQVar[I, F] {
    def qvarOf[A](a: A): I[QVar[F, A]] = MVar.in[I, F, A](a).map(QVarByMVar(_))
    def qvarEmpty[A]: I[QVar[F, A]]    = MVar.emptyIn[I, F, A].map(QVarByMVar(_))
  }
}
