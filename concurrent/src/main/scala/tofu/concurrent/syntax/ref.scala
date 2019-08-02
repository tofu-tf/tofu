package tofu.concurrent.syntax
import cats.{Functor, Monad}
import cats.data.OptionT
import cats.effect.Resource
import cats.effect.concurrent.Ref
import tofu.BracketThrow
import tofu.optics.{Contains, PProperty}
import cats.syntax.functor._
import cats.syntax.flatMap._
import tofu.concurrent.impl.FocusedRef

object ref {
  implicit class TofuRefOps[F[_], A](private val self: Ref[F, A]) extends AnyVal {

    def focused[B](focus: A Contains B)(implicit F: Functor[F]): Ref[F, B] = FocusedRef(self, focus)

    /** tries to avoid running `init` if state contains suitable value */
    def optimisticModify[B, X, R](prop: PProperty[A, A, R, X])(init: => F[X])(f: X => R)(implicit F: Monad[F]): F[R] =
      OptionT(self.get.map(prop.downcast)).getOrElseF(
        init.flatMap(x => self.modify(a => prop.downcast(a).fold((prop.set(a, x), f(x)))((a, _))))
      )

    /** tries to avoid initializing resource if state contains suitable value */
    def optimisticModifyRes[B, X, R](prop: PProperty[A, A, R, X])(init: => Resource[F, X])(f: X => R)(
        implicit F: BracketThrow[F]
    ): F[R] =
      OptionT(self.get.map(prop.downcast)).getOrElseF(
        init.use(x => self.modify(a => prop.downcast(a).fold((prop.set(a, x), f(x)))((a, _))))
      )
  }
}
