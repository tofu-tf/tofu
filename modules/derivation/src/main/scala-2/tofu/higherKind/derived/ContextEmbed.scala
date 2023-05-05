package tofu.higherKind.derived
import cats.FlatMap
import tofu.bi.TwinContext
import tofu.control.Bind
import tofu.higherKind.Embed
import tofu.higherKind.bi.EmbedBK
import tofu.kernel.types.HasContext

/** simple mixin for typeclass companion to add contextual embedded instance
  */
trait ContextEmbed[U[f[_]]] {
  final implicit def contextEmbed[F[_]: FlatMap](implicit FH: F HasContext U[F], UE: Embed[U]): U[F] =
    UE.embed(FH.context)
}

/** mixin for something related to your monad for example context datatype companion
  */
trait EmbeddedInstances[F[_]] {
  final implicit def contextEmbed[U[_[_]]](implicit FH: F HasContext U[F], UE: Embed[U], F: FlatMap[F]): U[F] =
    UE.embed(FH.context)
}

/** simple mixin for typeclass companion to add contextual embedded instance
  */
trait ContextBiEmbed[U[f[_, _]]] {
  final implicit def contextEmbed[F[+_, +_]: Bind](implicit FH: F TwinContext U[F], UE: EmbedBK[U]): U[F] =
    UE.biembed[F](FH.context)
}

/** mixin for something related to your monad for example context datatype companion
  */
trait BiEmbeddedInstances[F[+_, +_]] {
  final implicit def contextEmbed[U[f[_, _]]](implicit FH: F TwinContext U[F], UE: EmbedBK[U], F: Bind[F]): U[F] =
    UE.biembed[F](FH.context)
}
