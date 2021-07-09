package tofu.syntax.streams

import tofu.streams.{Compile, internal}

private[syntax] final class CompileOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
  def fold[B](init: B)(f: (B, A) => B)(implicit c: Compile[F, G]): G[B]           = c.fold(fa)(init)(f)
  def drain(implicit c: Compile[F, G]): G[Unit]                                   = c.drain(fa)
  def to[C[_]](implicit c: Compile[F, G], ev: internal.Factory[A, C[A]]): G[C[A]] = c.to[C, A](fa)
}

private[syntax] trait CompileSyntax {
  implicit def toCompileOps[F[_], G[_], A](fa: F[A]): CompileOps[F, G, A] = new CompileOps(fa)
}
