package tofu.higherKind.bi

import tofu.control.Bind

trait EmbedB[U[f[_, _]]] {
  def biembed[F[_, _]: Bind](fu: F[U[F], U[F]]): U[F]
}

object EmbedB {
  def apply[U[f[_, _]]](implicit u: EmbedB[U]): EmbedB[U] = u

  def of[F[_, _]: Bind, U[f[_, _]]](uf: F[U[F], U[F]])(implicit embed: EmbedB[U]): U[F] = 
    embed.biembed(uf)
}
