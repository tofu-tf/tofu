package tofu.examples.context.server
import cats.{Applicative, Defer, Functor, Monad}
import cats.effect.{ConcurrentEffect, ContextShift, Resource, Sync, Timer}
import derevo.derive
import derevo.circe.{decoder, encoder}
import org.http4s.{EntityDecoder, HttpApp, HttpRoutes}
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.{Router, Server}
import org.http4s.server.blaze.BlazeServerBuilder
import tofu.MonadThrow
import zio.{Task, UIO}
import zio.internal.Platform
import zio.interop.catz._
import zio.interop.catz.implicits._
import tofu.syntax.monadic._
import cats.syntax.applicativeError._
import cats.syntax.monadError._
import cats.syntax.option._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import tofu.syntax.foption._
import tofu.concurrent.Atom._
import tofu.concurrent.MakeAtom
import tofu.generate.GenUUID

import scala.collection.mutable
import scala.concurrent.ExecutionContext.global

object Main extends CatsApp {
  override val platform: Platform =
    Platform.default.withReportFailure(_ => ())

  def makeApp[I[_]: Sync, F[_]: Sync]: I[HttpApp[F]] = {
    val validator: RecipeValidate[F]       = RecipeValidate.make[F]
    val repository: I[RecipeRepository[F]] = RecipeRepository.make[I, F]
    val recipeService: I[RecipeService[F]] = repository.map(RecipeService.make[F](validator, _))
    val recipeRoutes: I[HttpRoutes[F]]     = recipeService.map(routes.make[F](_))
    recipeRoutes.map(rs => Router("/" -> rs).orNotFound)
  }

  def run(args: List[String]) = {
    makeApp[Task, Task]
      .flatMap(app => server.make[Task](app).use(_ => Task.never))
      .catchAllCause(_ => UIO.unit)
      .as(zio.ExitCode.success)
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

object Recipe {
  type Ingredient = String
  type Id         = String
}
@derive(decoder, encoder)
case class Price(currency: String, price: Double)
@derive(decoder, encoder)
case class Recipe(
    name: String,
    ingredients: List[Recipe.Ingredient],
    author: String,
    servingsAmount: Int,
    estimatedPrice: Price
)

trait RecipeValidate[F[_]] {
  def validate(recipe: Recipe): F[Boolean]
}

object RecipeValidate {
  def make[F[_]: Applicative]: RecipeValidate[F] =
    (recipe: Recipe) => (recipe.author != "" && recipe.estimatedPrice.price > 0 && recipe.ingredients.nonEmpty).pure[F]
}

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

object routes {
  def make[F[_]: Defer: MonadThrow: EntityDecoder[*[_], Recipe]](
      recipeService: RecipeService[F]
  ): HttpRoutes[F] = {
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
