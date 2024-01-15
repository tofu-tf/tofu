package tofu.interop

import cats.effect.kernel.Ref
import tofu.syntax.monadic._
import tofu.concurrent.impl.QVarSM
import cats.effect.kernel.Deferred
import cats.effect.kernel.GenConcurrent

//this implementation is really questionable
final class QVarCE3[F[_], A](ref: Ref[F, QVarSM.State[A, Deferred[F, A]]])(implicit F: GenConcurrent[F, ?])
    extends QVarSM[F, A, Deferred[F, A]] {
  protected def newPromise: F[Deferred[F, A]] = Deferred[F, A]

  protected def complete(x: A, promise: Deferred[F, A]): F[Unit] = promise.complete(x).void

  protected def await(p: Deferred[F, A]): F[A] = p.get

  protected def modifyF[X](f: S => (S, F[X])): F[X] = ref.modify(f).flatten

  protected def get: F[S] = ref.get

  protected def set(s: S): F[Unit] = ref.set(s)

  protected def onCancel(fa: F[A], fc: => F[Unit]): F[A] = F.onCancel(fa, fc)

}
