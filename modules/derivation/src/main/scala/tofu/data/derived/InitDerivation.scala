package tofu.data.derived
import cats.Monad
import derevo.Derivation
import magnolia.{CaseClass, Magnolia}
import mercator.Monadic
import tofu.Init

class InitDerivation[F[_]: Monad] extends Derivation[Init[F, *]] {
  private[this] implicit val magnoliaMonad: Monadic[F] = new MerkatorFromCats[F]

  type Typeclass[A] = Init[F, A]

  def combine[X](cc: CaseClass[Typeclass, X]): Init[F, X] =
    new Init[F, X] {
      def init: F[X] = cc.constructMonadic[F, Any](_.typeclass.init.asInstanceOf[F[Any]])
    }

  def instance[X]: Init[F, X] = macro Magnolia.gen[X]
}
