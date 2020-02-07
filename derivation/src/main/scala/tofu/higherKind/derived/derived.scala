package tofu.higherKind.derived
import derevo.{Derevo, delegating, DerivationK2}
import tofu.higherKind.{Embed, RepresentableK}

@delegating("tofu.higherKind.derived.genRepresentableK")
object representableK extends DerivationK2[RepresentableK] {
  def instance[T[_[_]]]: RepresentableK[T] = macro Derevo.delegateK2[RepresentableK, T]
}
@delegating("tofu.higherKind.derived.genEmbed")
object embed extends DerivationK2[Embed] {
  def instance[T[_[_]]]: Embed[T] = macro Derevo.delegateK2[Embed, T]
}
