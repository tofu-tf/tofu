package tofu.streams.syntax

import tofu.streams.Chunks

object chunks {

  implicit final class ChunksOps[F[_], A](private val fa: F[A])(implicit ch: Chunks[F]) {
    def chunks: F[List[A]]                       = ch.chunks(fa)
    def mapChunks[B](f: List[A] => List[B]): F[B] = ch.mapChunks(fa)(f)
  }
}
