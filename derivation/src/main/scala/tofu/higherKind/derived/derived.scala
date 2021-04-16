package tofu.higherKind.derived
import derevo.DerivationK2
import tofu.higherKind.{Embed, RepresentableK}
import derevo.DerivationKN11
import tofu.higherKind.bi.RepresentableB
import tofu.higherKind.bi.EmbedBK

object representableK extends DerivationK2[RepresentableK] {
  def instance[T[_[_]]]: RepresentableK[T] = macro HigherKindedMacros.representableK[T]
}

object embed extends DerivationK2[Embed] {
  def instance[T[_[_]]]: Embed[T] = macro HigherKindedMacros.embed[T]
}

object representableB extends DerivationKN11[RepresentableB] {
  def instance[T[bf[_, _]]]: RepresentableB[T] = macro HigherKindedMacros.representableB[T]
}

object biembed extends DerivationKN11[EmbedBK] {
  def instance[T[fb[_, _]]]: EmbedBK[T] = macro HigherKindedMacros.embedB[T]
}
