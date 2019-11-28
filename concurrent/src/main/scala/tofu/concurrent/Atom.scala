package tofu.concurrent
import cats.effect.Sync
import cats.effect.concurrent.Ref
import tofu.concurrent.Atom.AtomByRef
import tofu.higherKind.{RepresentableK, derived}
import tofu.syntax.monadic._

/** a middleground between cats.concurrent.Ref and zio.Ref */
trait Atom[F[_], A] {

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

  final case class AtomByRef[F[_], A](ref: Ref[F, A]) extends Atom[F, A] {
    override def get: F[A]                       = ref.get
    override def set(a: A): F[Unit]              = ref.set(a)
    override def getAndSet(a: A): F[A]           = ref.getAndSet(a)
    override def update(f: A => A): F[Unit]      = ref.update(f)
    override def modify[B](f: A => (A, B)): F[B] = ref.modify(f)
  }
}

trait MakeAtom[I[_], F[_]] {
  def atom[A](a: A): I[Atom[F, A]]
}

object MakeAtom {
  def apply[I[_], F[_]](implicit makeAtom: MakeAtom[I, F]) = new Applier[I, F](makeAtom)

  class Applier[I[_], F[_]](val makeAtom: MakeAtom[I, F]) extends AnyVal {
    def of[A](a: A): I[Atom[F, A]] = makeAtom.atom(a)
  }

  implicit def syncInstance[I[_]: Sync, F[_]: Sync]: MakeAtom[I, F] = new MakeAtom[I, F] {
    def atom[A](a: A): I[Atom[F, A]] = Ref.in[I, F, A](a).map(AtomByRef(_))
  }
}
