package tofu.streams.syntax

import tofu.streams.{Compile, Consume}

object compile {

  implicit final class CompileOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
    def fold[B](init: B)(f: (B, A) => B)(implicit c: Compile[F, G]): G[B] = c.fold(fa)(init)(f)
    def drain(implicit c: Compile[F, G]): G[Unit]                         = c.drain(fa)
  }

  implicit final class ConsumeOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
    def to[C[_]](implicit ev: Consume[F, G, C]): G[C[A]] = ev.consume(fa)
  }
}
