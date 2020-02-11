package tofu.data.derived
import cats.Monad
import magnolia.{CaseClass, Magnolia}
import mercator.Monadic

/**
  * Initialize value of type A in type F
  *
  * @tparam F initialization effect type
  * @tparam A result type
  */
trait Init[F[_], A] {
  def init: F[A]
}

class InitDerivation[F[_]: Monad] {
  private[this] implicit val magnoliaMonad: Monadic[F] = new MerkatorFromCats[F]

  type Typeclass[A] = Init[F, A]

  def combine[X](cc: CaseClass[Typeclass, X]): Init[F, X] =
    new Init[F, X] {
      def init: F[X] = cc.constructMonadic[F, Any](_.typeclass.init.asInstanceOf[F[Any]])
    }

  def instance[X]: Init[F, X] = macro Magnolia.gen[X]
}
