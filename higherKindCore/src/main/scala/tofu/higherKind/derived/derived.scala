package tofu.higherKind

package object derived {
  def genRepresentableK[Alg[_[_]]]: RepresentableK[Alg] = macro HigherKindedMacros.representableK[Alg]

  def genEmbed[Alg[_[_]]]: Embed[Alg] = macro HigherKindedMacros.embed[Alg]
}
