package tofu.higherKind

import cats.FlatMap
import cats.free.Free
import simulacrum.typeclass

import scala.annotation.nowarn

@typeclass @nowarn("cat=unused-imports")
trait Embed[U[_[_]]] {
  def embed[F[_]: FlatMap](ft: F[U[F]]): U[F]
}

object Embed extends EmbedInstanceChain[Embed] {
  def of[U[_[_]], F[_]: FlatMap](fuf: F[U[F]])(implicit embed: Embed[U]): U[F] = embed.embed(fuf)
}

trait EmbedInstanceChain[TC[u[_[_]]] >: Embed[u]] extends RepresentableKInstanceChain[TC] {
  private[this] def freeInstance[A]: Embed[Free[*[_], A]] = new Embed[Free[*[_], A]] {
    def embed[F[_]: FlatMap](ft: F[Free[F, A]]): Free[F, A] = Free.roll(ft)
  }

  private[this] val freeEmbedAny = freeInstance[Any]

  final implicit def freeEmbed[A]: TC[Free[*[_], A]] = freeEmbedAny.asInstanceOf[TC[Free[*[_], A]]]

}
