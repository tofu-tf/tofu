package tofu.examples.context.server
import cats.{Applicative, Defer, Functor, Monad}
import cats.effect.{ConcurrentEffect, ContextShift, Resource, Sync, Timer}
import derevo.derive
import derevo.circe.{decoder, encoder}
import org.http4s.{EntityDecoder, HttpApp, HttpRoutes}
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Server
import org.http4s.server.blaze.BlazeServerBuilder
import tofu.MonadThrow
import tofu.examples.context.server.recipe.Id
import zio.{Task, UIO}
import zio.internal.Platform
import zio.interop.catz._
import zio.interop.catz.implicits._
import tofu.syntax.monadic._
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import cats.syntax.option._
import tofu.syntax.foption._
import tofu.concurrent.Atom._
import tofu.concurrent.MakeAtom
import tofu.generate.GenUUID

import scala.concurrent.ExecutionContext.global

object Main extends CatsApp {
  override val platform: Platform =
    Platform.default.withReportFailure(_ => ())

  def run(args: List[String]) = {
    server.make[Task](???).use(_ => Task.never).catchAllCause(_ => UIO.unit).as(zio.ExitCode.success)
  }
}

object server {

  def make[F[_]: ConcurrentEffect: Timer: ContextShift](app: HttpApp[F]): Resource[F, Server[F]] = {
    BlazeServerBuilder[F](global)
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(app)
      .resource
  }
}

object recipe {
  type Ingredient = String
  type Id         = String
}
@derive(decoder, encoder)
case class Price(currency: String, price: Double)
@derive(decoder, encoder)
case class Recipe(
    name: String,
    ingredients: List[recipe.Ingredient],
    author: String,
    servingsAmount: Int,
    estimatedPrice: Price
)

trait RecipeValidate[F[_]]   {
  def validate(recipe: Recipe): F[Boolean]
}
trait RecipeRepository[F[_]] {
  def get(id: recipe.Id): F[Recipe]
  def add(recipe: Recipe): F[String]
}

object RecipeRepository {
  def make[I[_]: Functor, F[_]: MonadThrow: GenUUID](
      makeAtom: MakeAtom[I, F]
  ): I[RecipeRepository[F]] =
    makeAtom
      .atom(Map.empty[recipe.Id, Recipe])
      .map(atom =>
        new RecipeRepository[F] {
          override def add(recipe: Recipe): F[String] =
            for {
              id <- GenUUID.randomString[F]
              _  <- atom.update(_ + (id -> recipe))
            } yield id

          override def get(id: recipe.Id): F[Recipe] =
            atom.get.map(_.get(id)).getOrElseF(new Exception("not found").raiseError[F, Recipe])
        }
      )
}

trait RecipeService[F[_]] {
  def addRecipe(recipe: Recipe): F[Option[String]]
  def getRecipe(id: recipe.Id): F[Option[Recipe]]
}

object RecipeService {
  def make[F[_]: MonadThrow](recipeValidate: RecipeValidate[F], recipeRepository: RecipeRepository[F]): RecipeService[F] =
    new RecipeService[F] {
      override def addRecipe(recipe: Recipe): F[Option[String]] =
        recipeValidate.validate(recipe).ifM(recipeRepository.add(recipe).map(_.some), noneF[F, String])

      override def getRecipe(id: recipe.Id): F[Option[Recipe]] =
        recipeRepository.get(id).redeemWith(_ => noneF[F, Recipe], _.someF[F])
    }
}

object routes {
  def make[F[_]: Defer: MonadThrow: EntityDecoder[*[_], Recipe]](
      recipeService: RecipeService[F]
  ): HttpRoutes[F] =  {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {

      case GET -> Root / "recipes" / id =>
        for {
          recipe   <- recipeService.getRecipe(id)
          response <- recipe.fold(NotFound.apply("No recipe found"))(Ok.apply(_))
        } yield response

      case req @ POST -> Root / "recipes" =>
        for {
          recipe   <- req.as[Recipe]
          id       <- recipeService.addRecipe(recipe)
          response <- id.fold(Conflict("Recipe is already present"))(Ok.apply(_))
        } yield response

    }
  }
}
