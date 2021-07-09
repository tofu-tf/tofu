package tofu.syntax.streams

import tofu.streams.Chunks

private[syntax] final class ChunksOps[F[_], C[_], A](fa: F[A])(implicit ch: Chunks[F, C]) {
  def cons(ca: C[A]): F[A]                = ch.cons(fa)(ca)
  def chunks: F[C[A]]                     = ch.chunks(fa)
  def chunksN(n: Int): F[C[A]]            = ch.chunkN(fa)(n)
  def mapChunks[B](f: C[A] => C[B]): F[B] = ch.mapChunks(fa)(f)
}

private[syntax] trait ChunkSyntax {
  implicit def toChunkOps[F[_], C[_], A](fa: F[A])(implicit ch: Chunks[F, C]): ChunksOps[F, C, A] = new ChunksOps(fa)
}
