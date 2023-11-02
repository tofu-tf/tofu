package tofu.interop

import tofu.lift.Unlift
import cats.effect.std.Dispatcher
import tofu.WithContext
import cats.effect.Async
import cats.Apply

abstract class ContextDispatch[F[_]] {
  type Base[_]
  def unlift: Unlift[Base, F]
  def dispatcher: WithContext[F, Dispatcher[Base]]
  def async: Async[Base]
  def apply: Apply[F]
}

object ContextDispatch {

  final class Impl[F[_], B[_]](implicit
      val dispatcher: WithContext[F, Dispatcher[B]],
      val unlift: Unlift[B, F],
      val async: Async[B],
      val apply: Apply[F],
  ) extends ContextDispatch[F] {
    final type Base[A] = B[A]
  }

  implicit def resolveContextDispatch[F[_]: Apply, B[_]](implicit
      context: WithContext[F, Dispatcher[B]],
      B: Async[B],
      unlift: Unlift[B, F],
  ): Impl[F, B] = new Impl[F, B]
}
