package tofu.higherKind

package object derived {
  def genRepresentableK[Alg[_[_]]]: RepresentableK[Alg] = macro HigherKindedMacros.representableK[Alg]
}
