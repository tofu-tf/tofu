package tofu.concurrent
import cats.{Applicative, Functor}
import cats.effect.Sync
import cats.effect.concurrent.Ref
import tofu.Guarantee
import tofu.concurrent.Atom.AtomByRef
import tofu.higherKind.{RepresentableK, derived}
import tofu.optics.Contains
import tofu.syntax.monadic._
import tofu.syntax.bracket._

/** a middleground between cats.concurrent.Ref and zio.Ref */
trait Atom[+F[_], A] {

  /**
    * Obtains the current value.
    *
    * Since `Ref` is always guaranteed to have a value, the returned action
    * completes immediately after being bound.
    */
  def get: F[A]

  /**
    * Sets the current value to `a`.
    *
    * The returned action completes after the reference has been successfully set.
    *
    * Satisfies:
    *   `r.set(fa) *> r.get == fa`
    */
  def set(a: A): F[Unit]

  /**
    * Replaces the current value with `a`, returning the previous value.
    */
  def getAndSet(a: A): F[A]

  /**
    * Modifies the current value using the supplied update function. If another modification
    * occurs between the time the current value is read and subsequently updated, the modification
    * is retried using the new value. Hence, `f` may be invoked multiple times.
    *
    * Satisfies:
    *   `r.update(_ => a) == r.set(a)`
    */
  def update(f: A => A): F[Unit]

  /**
    * Like `tryModify` but does not complete until the update has been successfully made.
    */
  def modify[B](f: A => (A, B)): F[B]
}

object Atom {
  implicit def representableKInstance[A]: RepresentableK[Atom[*[_], A]] = derived.genRepresentableK[Atom[*[_], A]]

  final implicit class AtomOps[F[_], A](private val self: Atom[F, A]) extends AnyVal {
    def focused[B](bInA: A Contains B)(implicit F: Functor[F]): Atom[F, B] = self match {
      case FocusedAtom(v, focus) => FocusedAtom(v, focus >> bInA)
      case _                     => FocusedAtom(self, bInA)
    }
  }

  final case class AtomByRef[F[_], A](ref: Ref[F, A]) extends Atom[F, A] {
    override def get: F[A]                       = ref.get
    override def set(a: A): F[Unit]              = ref.set(a)
    override def getAndSet(a: A): F[A]           = ref.getAndSet(a)
    override def update(f: A => A): F[Unit]      = ref.update(f)
    override def modify[B](f: A => (A, B)): F[B] = ref.modify(f)
  }

  final case class QAtom[F[_]: Applicative: Guarantee, A](qvar: QVar[F, A]) extends Atom[F, A] {
    def get: F[A]                  = qvar.read
    def set(a: A): F[Unit]         = getAndSet(a).void
    def getAndSet(a: A): F[A]      = qvar.take.guaranteeAlways(qvar.put(a))
    def update(f: A => A): F[Unit] = qvar.take.bracketIncomplete(a => qvar.put(f(a)))(qvar.put)
    def modify[B](f: A => (A, B)): F[B] = qvar.take.bracketIncomplete { a =>
      val (next, res) = f(a)
      qvar.put(next) as res
    }(qvar.put)
  }

  private[Atom] case class FocusedAtom[F[_]: Functor, A, B](v: Atom[F, A], focus: Contains[A, B]) extends Atom[F, B] {
    def get: F[B]                  = v.get.map(focus.get)
    def set(b: B): F[Unit]         = v.update(focus.set(_, b))
    def getAndSet(b: B): F[B]      = v.modify(a => (focus.set(a, b), focus.get(a)))
    def update(f: B => B): F[Unit] = v.update(focus.update(_, f))
    def modify[C](f: B => (B, C)): F[C] = v.modify { a =>
      val (b, c) = f(focus.get(a))
      (focus.set(a, b), c)
    }
  }
}

trait MakeAtom[I[_], F[_]] {
  def atom[A](a: A): I[Atom[F, A]]
}

object MakeAtom {
  def apply[I[_], F[_]](implicit makeAtom: MakeAtom[I, F]) = new Applier[I, F](makeAtom)

  final class Applier[I[_], F[_]](private val makeAtom: MakeAtom[I, F]) extends AnyVal {
    def of[A](a: A): I[Atom[F, A]] = makeAtom.atom(a)
  }

  implicit def syncInstance[I[_]: Sync, F[_]: Sync]: MakeAtom[I, F] = new MakeAtom[I, F] {
    def atom[A](a: A): I[Atom[F, A]] = Ref.in[I, F, A](a).map(AtomByRef(_))
  }
}
