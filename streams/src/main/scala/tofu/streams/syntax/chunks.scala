package tofu.streams.syntax

import tofu.streams.Chunks

object chunks {

  implicit final class ChunksOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def chunks(implicit ch: Chunks[F]): F[List[A]]                        = ch.chunks(fa)
    def mapChunks[B](f: List[A] => List[B])(implicit ch: Chunks[F]): F[B] = ch.mapChunks(fa)(f)
  }
}
