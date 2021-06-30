package tofu.lift

import cats.FlatMap
import cats.tagless.{FunctorK, InvariantK}
import tofu.higherKind.Embed
import tofu.syntax.liftKernel._

trait Rebase[U[_[_]]] {
  def rebase[F[_], G[_]: FlatMap](uf: U[F])(implicit FG: Unlift[F, G]): U[G]
}

object Rebase extends LowPriorRebase {
  def apply[U[_[_]]](implicit instance: Rebase[U]): Rebase[U] = instance

  implicit def byFunctorK[U[_[_]]: FunctorK]: Rebase[U] = new Rebase[U] {
    def rebase[F[_], G[_]: FlatMap](uf: U[F])(implicit FG: Unlift[F, G]): U[G] = uf.lift
  }
}

trait LowPriorRebase {
  final implicit def byEmbedInvariant[U[_[_]]: Embed: InvariantK]: Rebase[U] = new Rebase[U] {
    def rebase[F[_], G[_]: FlatMap](uf: U[F])(implicit FG: Unlift[F, G]): U[G] = Embed.of(uf.unlift)
  }
}
