package tofu

import cats.MonadError
import scala.annotation.unused
import tofu.internal.{EffectComp, Effect2Comp}
import tofu.internal.carriers.FinallyCarrier

/** Bracket-like typeclass allowing to understand if operation was succeed
  * @tparam F effect process
  */
trait Guarantee[F[_]] {

  /** guarantee of finalization
    * @param init initialization process, probably acquiring some resources, need for finalization
    * @param action function, using initialized result, that could complete with some error or be canceled
    * @param release finalization, that will get initialized value and flag of `action` success
    */
  def bracket[A, B, C](init: F[A])(action: A => F[B])(release: (A, Boolean) => F[C]): F[B]
}

object Guarantee extends EffectComp[Guarantee] {
  final implicit def fromBracket[F[_], E, Exit[_]](implicit
      @unused ev1: MonadError[F, E],
      carrier: FinallyCarrier.Aux[F, E, Exit]
  ): Finally[F, Exit] =
    carrier.content
}

/** Bracket-like typeclass allowing to match exit of the process
  * @tparam F effect process
  * @tparam Exit structure, describing process exit like `ExitCase` or `Exit`
  */
trait Finally[F[_], Exit[_]] extends Guarantee[F] {

  /** guarantee of finalization
    * @param init initialization process, probably acquiring some resources, need for finalization
    * @param action function, using initialized result, that could complete with some error or be canceled
    * @param release finalization, that will get initialized value and structure, describing exit of `action`
    */
  def finallyCase[A, B, C](init: F[A])(action: A => F[B])(release: (A, Exit[B]) => F[C]): F[B]
}

object Finally extends Effect2Comp[Finally]
