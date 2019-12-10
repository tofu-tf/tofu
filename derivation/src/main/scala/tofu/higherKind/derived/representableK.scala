package tofu.higherKind.derived
import org.manatki.derevo.{Derevo, DerivationK2, delegating}
import tofu.higherKind.RepresentableK

@delegating("tofu.higherKind.derived.genRepresentableK")
object representableK extends DerivationK2[RepresentableK] {
  def instance[T[_[_]]]: RepresentableK[T] = macro Derevo.delegateK2[RepresentableK, T]
}
