package tofu.higherKind.bi

import tofu.control.Bind

trait EmbedBK[U[f[_, _]]] {
  def biembed[F[_, _]: Bind](fu: F[U[F], U[F]]): U[F]
}

object EmbedBK {
  def apply[U[f[_, _]]](implicit u: EmbedBK[U]): EmbedBK[U] = u

  def of[F[_, _]: Bind, U[f[_, _]]](uf: F[U[F], U[F]])(implicit embed: EmbedBK[U]): U[F] =
    embed.biembed(uf)
}
