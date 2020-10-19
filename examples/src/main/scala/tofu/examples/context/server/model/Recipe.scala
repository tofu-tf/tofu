package tofu.examples.context.server.model

import derevo.circe.{decoder, encoder}
import derevo.derive

object Recipe {
  type Ingredient = String
  type Id         = String
}

@derive(decoder, encoder)
case class Recipe(
    name: String,
    ingredients: List[Recipe.Ingredient],
    author: String,
    servingsAmount: Int,
    estimatedPrice: Price
)

@derive(decoder, encoder)
case class Price(currency: String, price: Double)
