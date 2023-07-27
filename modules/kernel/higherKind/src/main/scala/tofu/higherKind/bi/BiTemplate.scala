package tofu.higherKind.bi
import BiTemplate.No

trait BiTemplate[F[_, _]] {
  type FS[A] = F[No, A]
}

object BiTemplate {
  type No <: Nothing

}
