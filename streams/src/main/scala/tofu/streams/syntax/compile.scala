package tofu.streams.syntax

import tofu.streams.Compile

object compile {

  implicit final class CompileOps[F[_], G[_], A](fa: F[A])(implicit com: Compile[F, G]) {
    def compile: G[List[A]] = com.compile(fa)
  }
}
