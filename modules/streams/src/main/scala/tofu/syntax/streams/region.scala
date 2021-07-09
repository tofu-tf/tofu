package tofu.syntax.streams

import tofu.streams.Region

private[syntax] final class RegionPA[F[_]](val __ : Boolean) extends AnyVal {
  def apply[G[_], Exit, R](open: G[R])(close: R => G[Unit])(implicit F: Region[F, G, Exit]): F[R] =
    F.region(open)(close)
}

private[syntax] final class RegionCasePA[F[_]](val __ : Boolean) extends AnyVal {
  def apply[G[_], Exit, R](open: G[R])(close: (R, Exit) => G[Unit])(implicit F: Region[F, G, Exit]): F[R] =
    F.regionCase(open)(close)
}

private[syntax] trait RegionSyntax {

  def region[F[_]]: RegionPA[F] = new RegionPA[F](true)

  def regionCase[F[_]]: RegionCasePA[F] = new RegionCasePA[F](true)
}
