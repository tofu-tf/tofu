package tofu.streams

import cats.effect.ExitCase

trait Region[F[_], G[_], E] {

  /** Open some resource `R` acquired by an effect `G`
    * ensuring it will be released once `F` is completed or interrupted.
    */
  def regionCase[R](open: G[R])(close: (R, ExitCase[E]) => G[Unit]): F[R]

  /** Like [[regionCase]] but does not provide an access to the [[ExitCase]] in `close` section.
    */
  final def region[R](open: G[R])(close: R => G[Unit]): F[R] =
    regionCase(open) { case (r, _) => close(r) }
}
