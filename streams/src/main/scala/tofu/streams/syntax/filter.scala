package tofu.streams.syntax

import cats.FunctorFilter

object filter {

  implicit final class StreamFilterOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def unNone[A0](implicit ev: A <:< Option[A0], ff: FunctorFilter[F]): F[A0] =
      ff.collect(ff.functor.map(fa)(ev)) { case Some(a0) => a0 }
  }
}
