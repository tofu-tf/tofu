package tofu.syntax.streams

import cats.FunctorFilter

private[syntax] final class StreamFilterOps[F[_], A](private val fa: F[A]) extends AnyVal {
  def unNone[A0](implicit ev: A <:< Option[A0], ff: FunctorFilter[F]): F[A0] =
    ff.collect(ff.functor.map(fa)(ev)) { case Some(a0) => a0 }
}

private[syntax] trait StreamFilterSyntax {
  implicit def toStreamFilterOps[F[_], A](fa: F[A]): StreamFilterOps[F, A] = new StreamFilterOps(fa)
}
