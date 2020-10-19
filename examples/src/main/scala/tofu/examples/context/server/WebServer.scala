package tofu.examples.context.server
import java.util.UUID

import cats.Defer
import cats.effect.{ConcurrentEffect, ContextShift, Resource, Sync, Timer}
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.{EntityDecoder, HttpApp, HttpRoutes}
import tofu.{MonadThrow, WithRun}
import tofu.examples.context.server.model._
import tofu.generate.GenUUID
import tofu.syntax.monadic._
import zio.internal.Platform
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{ExitCode, RIO, Task, UIO, URIO}
import tofu.syntax.context._
import tofu.zioInstances.implicits._

import scala.concurrent.ExecutionContext.global

object Main extends CatsApp {
  override val platform: Platform =
    Platform.default.withReportFailure(_ => ())

  def makeApp[I[_]: Sync, F[_]: Sync : WithRun[*[_], I, RecipeTrace]]: I[HttpApp[I]] = {
    val validator: RecipeValidate[F] = RecipeValidate.make[F]
    for {
      recipeRepository <- RecipeRepository.make[I, F]
      recipeService     = RecipeService.make[F](validator, recipeRepository)
      recipeRoutes      = routes.make[I, F](recipeService)
    } yield Router("/" -> recipeRoutes).orNotFound
  }

  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    makeApp[Task, RIO[RecipeTrace, *]]
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

final case class RecipeTrace(id: UUID)


object routes {
  def make[F[_]: Defer: MonadThrow: EntityDecoder[*[_], Recipe]: GenUUID, G[_]: WithRun[*[_], F, RecipeTrace]](
      recipeService: RecipeService[G]
  ): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {

      case GET -> Root / "recipes" / id =>
        for {
          traceId <- GenUUID.random[F]
          trace = RecipeTrace(traceId)
          recipe   <- runContext(recipeService.getRecipe(id))(trace)
          response <- recipe.fold(NotFound.apply("No recipe found"))(Ok.apply(_))
        } yield response

      case req @ POST -> Root / "recipes" =>
        for {
          recipe <- req.as[Recipe]
          traceId <- GenUUID.random[F]
          trace = RecipeTrace(traceId)
          id       <-runContext(recipeService.addRecipe(recipe))(trace)
          response <- id.fold(Conflict("Recipe is already present"))(Ok.apply(_))
        } yield response

    }
  }
}
