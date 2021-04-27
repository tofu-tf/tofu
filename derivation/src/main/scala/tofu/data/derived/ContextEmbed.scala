package tofu.data.derived
import cats.FlatMap
import tofu.HasContext
import tofu.higherKind.Embed

/** simple mixin for typeclass companion
  * to add contextual embedded instance
  */
trait ContextEmbed[U[f[_]]] {
  final implicit def contextEmbed[F[_]: FlatMap](implicit FH: F HasContext U[F], UE: Embed[U]): U[F] =
    UE.embed(FH.context)
}

/** mixin for something related to your monad
  * for example context datatype companion
  */
trait EmbeddedInstances[F[_]] {
  final implicit def contextEmbed[U[_[_]]](implicit FH: F HasContext U[F], UE: Embed[U], F: FlatMap[F]): U[F] =
    UE.embed(FH.context)
}
