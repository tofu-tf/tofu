package tofu.streams

import cats.effect.ExitCase

trait Region[F[_], G[_], E] {

  /** Acquire some resource `R` acquired by an effect `G`
    * ensuring it will be released once `F` is completed or interrupted.
    */
  def regionCase[R](open: G[R])(close: (R, ExitCase[E]) => G[Unit]): F[R]

  /** Like [[regionCase]] but dose not provide an access to the [[ExitCase]] on `close` section.
    */
  def region[R](open: G[R])(close: R => G[Unit]): F[R] =
    regionCase(open) { case (r, _) => close(r) }
}

object Region {

  def apply[F[_], G[_], E](implicit ev: Region[F, G, E]): Region[F, G, E] = ev
}
