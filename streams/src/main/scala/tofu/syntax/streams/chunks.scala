package tofu.syntax.streams

import tofu.streams.Chunks

object chunks {

  implicit final class ChunksOps[F[_], C[_], A](fa: F[A])(implicit ch: Chunks[F, C]) {
    def chunks: F[C[A]]                     = ch.chunks(fa)
    def mapChunks[B](f: C[A] => C[B]): F[B] = ch.mapChunks(fa)(f)
  }
}
