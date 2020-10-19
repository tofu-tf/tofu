package tofu.examples.context.server

import cats.Applicative
import cats.syntax.applicative._
import tofu.examples.context.server.model.Recipe

trait RecipeValidate[F[_]] {
  def validate(recipe: Recipe): F[Boolean]
}

object RecipeValidate {
  def make[F[_]: Applicative]: RecipeValidate[F] =
    (recipe: Recipe) => (recipe.author != "" && recipe.estimatedPrice.price > 0 && recipe.ingredients.nonEmpty).pure[F]
}
