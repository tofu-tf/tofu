package tofu.streams.syntax

import tofu.streams.Chunks

object chunks {

  implicit final class ChunksOps[F[_], C[_], A](private val fa: F[A]) extends AnyVal {
    def chunks(implicit ch: Chunks[F, C]): F[C[A]]                     = ch.chunks(fa)
    def mapChunks[B](f: C[A] => C[B])(implicit ch: Chunks[F, C]): F[B] = ch.mapChunks(fa)(f)
  }
}
