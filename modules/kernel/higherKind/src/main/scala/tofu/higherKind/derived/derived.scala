package tofu.higherKind
import bi.{RepresentableB, EmbedBK}

package object derived {
  def genRepresentableK[Alg[_[_]]]: RepresentableK[Alg] = macro HigherKindedMacros.representableK[Alg]

  def genEmbed[Alg[_[_]]]: Embed[Alg] = macro HigherKindedMacros.embed[Alg]

  def genRepresentableB[Alg[_[_, _]]]: RepresentableB[Alg] = macro HigherKindedMacros.representableB[Alg]

  def genEmbedB[Alg[_[_, _]]]: EmbedBK[Alg] = macro HigherKindedMacros.embedB[Alg]

  def factorize[Builder, F[_], Alg[_[_]]](builder: Builder): Alg[F] =
    macro HigherKindedMacros.factorize[Builder, F, Alg]

  def bifactorize[Builder, F[_, _], Alg[_[_, _]]](builder: Builder): Alg[F] =
    macro HigherKindedMacros.bifactorize[Builder, F, Alg]
}
