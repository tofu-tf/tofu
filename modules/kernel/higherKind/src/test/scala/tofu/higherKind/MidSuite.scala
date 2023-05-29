package tofu.higherKind

import cats.tagless.{ApplyK, InvariantK}
import cats.{Monoid, Semigroup}
import tofu.higherKind.Mid._

object MidSuite {
  def summonMidInstances[F[_], U[_[_]]: ApplyK, W[_[_]]: MonoidalK](): Unit = {
    implicitly[Semigroup[U[Mid[F, _]]]]
    implicitly[Monoid[W[Mid[F, _]]]]
    implicitly[InvariantK[({ type L[x[_]] = Mid[x, Any] })#L]]
    ()
  }
}
