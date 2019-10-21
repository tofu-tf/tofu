package tofu

package object bi {
  type UContains[F[_, _], C]         = BiContext[F, Nothing, C]
  type ULocal[F[_, _], C]            = BiLocal[F, Nothing, C]
  type UProvide[F[_, _], G[_, _], C] = BiProvide[F, G, Nothing, C]
  type URun[F[_, _], G[_, _], C]     = BiRun[F, G, Nothing, C]

  type BiConst[A, B, C] = C

}
