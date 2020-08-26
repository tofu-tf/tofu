package tofu

package object streams {

  type RegionThrow[F[_], G[_]] = Region[F, G, Throwable]
}
