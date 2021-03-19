package tofu
package config

import scala.concurrent.{BlockContext, Future}

trait ConfigArr[U[_[_]], A] {
  def apply[F[_]: ConfigMonad](input: U[F]): F[A]
}

object ConfigArr {
  final case class Builder[U[_[_]], A, B](make: ConfigArr[U, A] => B) extends AnyVal {
    type T[_]
    def apply(f: ConfigMonad[T] => U[T] => T[A]): B = make(
      new ConfigArr[U, A] {
        def apply[F[_]](input: U[F])(implicit F: ConfigMonad[F]): F[A] =
          f(F.asInstanceOf[ConfigMonad[T]])(input.asInstanceOf[U[T]]).asInstanceOf[F[A]]
      }
    )
  }
}

trait ConfigFunc[A, B] extends ConfigArr[λ[f[_] => A], B] {
  def apply[F[_]: ConfigMonad](input: A): F[B]
}

object ConfigFunc {
  def apply[A, B] = ConfigArr.Builder[λ[`f[_]` => A], B, ConfigFunc[A, B]] { f =>
    new ConfigFunc[A, B] { def apply[F[_]: ConfigMonad](input: A): F[B] = f(input) }
  }
}
