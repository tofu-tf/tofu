package tofu.examples.context.server

import tofu.MonadThrow
import cats.Monad
import tofu.syntax.monadic._
import cats.syntax.applicativeError._
import tofu.syntax.foption._
import tofu.concurrent.MakeAtom
import tofu.examples.context.server.model.Recipe

trait RecipeRepository[F[_]] {
  def get(id: Recipe.Id): F[Recipe]
  def add(recipe: Recipe): F[String]
}

object RecipeRepository {
  def make[I[_]: Monad, F[_]: MonadThrow](implicit
      makeAtom: MakeAtom[I, F]
  ): I[RecipeRepository[F]] = for {
    idsAtom  <- MakeAtom[I, F].of(0)
    dataAtom <- MakeAtom[I, F].of(Map.empty[Recipe.Id, Recipe])
  } yield new RecipeRepository[F] {
    override def add(recipe: Recipe): F[String] =
      for {
        id <- idsAtom.get
        _  <- idsAtom.update(_ + 1)
        _  <- dataAtom.update(_ + (id.toString -> recipe))
      } yield id.toString

    override def get(id: Recipe.Id): F[Recipe] =
      dataAtom.get.map(_.get(id)).getOrElseF(new Exception("not found").raiseError[F, Recipe])
  }

}
