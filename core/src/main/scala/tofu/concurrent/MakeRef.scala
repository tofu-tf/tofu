package tofu.concurrent

import cats.effect.Sync
import cats.effect.concurrent.Ref

trait MakeRef[I[_], F[_]] {
  def refOf[A](a: A): I[Ref[F, A]]
}

object MakeRef {
  def apply[I[_], F[_]](implicit makeRef: MakeRef[I, F]) = new Applier[I, F](makeRef)

  class Applier[I[_], F[_]](val makeRef: MakeRef[I, F]) extends AnyVal {
    def of[A](a: A): I[Ref[F, A]] = makeRef.refOf(a)
  }

  implicit def syncInstance[I[_]: Sync, F[_]: Sync]: MakeRef[I, F] = new MakeRef[I, F] {
    def refOf[A](a: A): I[Ref[F, A]] = Ref.in[I, F, A](a)
  }
}
