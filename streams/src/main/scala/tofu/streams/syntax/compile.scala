package tofu.streams.syntax

import tofu.streams.Compile

object compile {

  implicit final class CompileOps[F[_], G[_], A](private val fa: F[A]) extends AnyVal {
    def compile(implicit ev: Compile[F, G]): G[ev.C[A]] = ev.compile(fa)
  }
}
