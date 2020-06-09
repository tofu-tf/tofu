package tofu.higherKind.derived
import derevo.{Derevo, delegating, DerivationK2}
import tofu.higherKind.{Embed, RepresentableK}
import tofu.higherKind.bi.{EmbedB, RepresentableB}

object representableK extends DerivationK2[RepresentableK] {
  def instance[T[_[_]]]: RepresentableK[T] = macro HigherKindedMacros.representableK[T]
}

object embed extends DerivationK2[Embed] {
  def instance[T[_[_]]]: Embed[T] = macro HigherKindedMacros.embed[T]
}
