package tofu.streams

trait Region[F[_], G[_], +Exit] {

  /** Open some resource `R` acquired by an effect `G`
    * ensuring it will be released once `F` is completed or interrupted.
    */
  def regionCase[R](open: G[R])(close: (R, Exit) => G[Unit]): F[R]

  /** Like [[regionCase]] but does not provide an access to the Exit in `close` section.
    */
  final def region[R](open: G[R])(close: R => G[Unit]): F[R] =
    regionCase[R](open) { case (r, _) => close(r) }
}

object Region {
  def apply[F[_], G[_], Exit](implicit ev: Region[F, G, Exit]): Region[F, G, Exit] = ev
}
