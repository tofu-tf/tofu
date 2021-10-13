package tofu.data

package object derived {
  type Merged[A]             = Merged.Mer[A]
  type ContextEmbed[U[f[_]]] = tofu.higherKind.derived.ContextEmbed[U]
}
