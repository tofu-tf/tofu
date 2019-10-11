package tofu.data
import cats.tagless.ApplyK
import tofu.higherKind.RepresentableK

package object derived {
  type Merged[A] = Merged.Mer[A]
  def genRepresentableK[Alg[_[_]]]: RepresentableK[Alg] = macro HigherKindedMacros.representableK[Alg]
}
