package tofu.streams.syntax

import tofu.streams.Compile

object compile {

  implicit final class CompileOps[F[_], G[_], C[_], A](private val fa: F[A]) extends AnyVal {
    def compile(implicit com: Compile[F, G, C]): G[C[A]] = com.compile(fa)
  }
}
