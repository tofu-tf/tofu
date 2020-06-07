package tofu.streams.syntax

import tofu.streams.Chunks

object chunks {

  implicit final class ChunksOps[F[_], A](private val fa: F[A])(implicit ch: Chunks[F]) {
    def chunks: F[Seq[A]]                       = ch.chunks(fa)
    def mapChunks[B](f: Seq[A] => Seq[B]): F[B] = ch.mapChunks(fa)(f)
  }
}
