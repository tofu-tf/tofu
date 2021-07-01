package tofu

import tofu.internal.Interop
import cats.MonadError

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

object Guarantee extends GuaranteeInstanceChain[Guarantee]

trait GuaranteeInstanceChain[T[f[_]] >: Guarantee[f]] extends FinallyInstanceChain[λ[(`f[_]`, `exit[_]`) => T[f]]]

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

object Finally extends FinallyInstanceChain[Finally]

trait FinallyInstanceChain[T[f[_], exit[_]] >: Finally[f, exit]] {
  // we need this MonadError evidence to infer `E`
  final implicit def fromBracket[F[_], E, Exit[_]](implicit ev1: MonadError[F, E]): T[F, Exit] =
    macro Interop.delegate1e1[Finally[F, Exit], F, E, { val `tofu.interop.CE2Kernel.finallyFromBracket`: Unit }]
}
