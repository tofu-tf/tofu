package tofu.bi

package object lift {
  type URUnlift[F[_, _], G[_, _]] = RUnlift[F, G, Nothing]
  type ULUnlift[F[_, _], G[_, _]] = LUnlift[F, G, Nothing]
}
