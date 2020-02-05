package tofu.higherKind.derived
import org.manatki.derevo.{Derevo, delegating}
import tofu.higherKind.RepresentableK
import derevo.DerivationK2

@delegating("tofu.higherKind.derived.genRepresentableK")
object representableK extends DerivationK2[RepresentableK] {
  def instance[T[_[_]]]: RepresentableK[T] = macro Derevo.delegateK2[RepresentableK, T]
}
