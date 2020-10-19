package tofu.examples.context.server

import tofu.MonadThrow
import tofu.syntax.monadic._
import tofu.syntax.foption._
import cats.syntax.option._
import cats.syntax.monadError._
import tofu.examples.context.server.model.Recipe

trait RecipeService[F[_]] {
  def addRecipe(recipe: Recipe): F[Option[String]]
  def getRecipe(id: Recipe.Id): F[Option[Recipe]]
}

object RecipeService {
  def make[F[_]: MonadThrow](
      recipeValidate: RecipeValidate[F],
      recipeRepository: RecipeRepository[F]
  ): RecipeService[F] =
    new RecipeService[F] {
      override def addRecipe(recipe: Recipe): F[Option[String]] =
        recipeValidate.validate(recipe).ifM(recipeRepository.add(recipe).map(_.some), noneF[F, String])

      override def getRecipe(id: Recipe.Id): F[Option[Recipe]] =
        recipeRepository.get(id).redeemWith(_ => noneF[F, Recipe], _.someF[F])
    }
}
