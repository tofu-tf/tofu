package tofu

import cats.MonadError
import tofu.internal.carriers.{FinallyCarrier2, FinallyCarrier3}
import tofu.internal.{Effect2Comp, EffectComp}

import scala.annotation.unused

/** Bracket-like typeclass allowing to understand if operation was succeed
  * @tparam F
  *   effect process
  */
trait Guarantee[F[_]] {

  /** guarantee of finalization
    * @param init
    *   initialization process, probably acquiring some resources, need for finalization
    * @param action
    *   function, using initialized result, that could complete with some error or be canceled
    * @param release
    *   finalization, that will get initialized value and flag of `action` success
    */
  def bracket[A, B, C](init: F[A])(action: A => F[B])(release: (A, Boolean) => F[C]): F[B]
}

object Guarantee extends EffectComp[Guarantee] with GuaranteeInstances0 {
  final implicit def interopCE3[F[_], E, Exit[_]](implicit
      @unused ev1: MonadError[F, E],
      carrier: FinallyCarrier3.Aux[F, E, Exit]
  ): Finally[F, Exit] =
    carrier.content
}

private[tofu] trait GuaranteeInstances0 {
  final implicit def interopCE2[F[_], E, Exit[_]](implicit
      @unused ev1: MonadError[F, E],
      carrier: FinallyCarrier2.Aux[F, E, Exit]
  ): Finally[F, Exit] =
    carrier.content
}

/** Bracket-like typeclass allowing to match exit of the process
  * @tparam F
  *   effect process
  * @tparam Exit
  *   structure, describing process exit like `ExitCase` or `Exit`
  */
trait Finally[F[_], Exit[_]] extends Guarantee[F] {

  /** guarantee of finalization
    * @param init
    *   initialization process, probably acquiring some resources, need for finalization
    * @param action
    *   function, using initialized result, that could complete with some error or be canceled
    * @param release
    *   finalization, that will get initialized value and structure, describing exit of `action`
    */
  def finallyCase[A, B, C](init: F[A])(action: A => F[B])(release: (A, Exit[B]) => F[C]): F[B]
}

object Finally extends Effect2Comp[Finally]
