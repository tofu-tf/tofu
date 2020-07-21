package tofu.streams.syntax

import tofu.streams.Compile

object compile {

  implicit final class CompileOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
    def to[C[_]](implicit c: Compile[F, G], ev: scala.collection.Factory[A, C[A]]): G[C[A]] = c.to(fa)
    def fold[B](init: B)(f: (B, A) => B)(implicit c: Compile[F, G]): G[B]                   = c.fold(fa)(init)(f)
    def drain(implicit c: Compile[F, G]): G[Unit]                                           = c.drain(fa)
  }
}
