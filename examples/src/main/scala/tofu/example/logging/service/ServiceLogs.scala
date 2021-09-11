package tofu.example.logging.service

import cats.Monad
import cats.data.ReaderT
import cats.effect._
import derevo.circe.decoder
import derevo.derive
import org.http4s._
import org.http4s.blaze.server._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router
import tofu.WithProvide
import tofu.logging.Logging
import tofu.logging.derivation.loggable
import tofu.syntax.context._
import tofu.syntax.foption._
import tofu.syntax.logging._
import tofu.syntax.monadic._

import scala.concurrent.duration.{Duration, DurationInt}

@derive(loggable, decoder)
case class Cargo(weight: Int, name: String)
@derive(loggable, decoder)
case class Destination(address: String, city: String)

trait CargoService[F[_]] {
  def move(cargo: Cargo, from: Destination, to: Destination): F[Duration]
  def deposit(cargo: Cargo, howLong: Duration): F[Unit]
}

object CargoService extends Logging.Companion[CargoService] {
  def make[F[_]: MonadThrow: Log](movingCompany: MovingCompany[F], warehouse: Warehouse[F]): CargoService[F] = {
    val serviceAddress = Destination("Cargo Street 51", "New Servexico")
    new CargoService[F] {
      override def move(cargo: Cargo, from: Destination, to: Destination): F[Duration] =
        info"Moving cargo $cargo from $from to $to" *>
          movingCompany
            .transport(cargo.weight, from, to)
            .orThrow(new Exception(s"Moving company has rejected the cargo $cargo"))

      override def deposit(cargo: Cargo, howLong: Duration): F[Unit] = for {
        _                <- info"Depositing cargo $cargo"
        isAcceptable     <- warehouse.requestStorage(cargo)
        warehouseAddress <- warehouse.address
        _                <- movingCompany
                              .transport(cargo.weight, serviceAddress, warehouseAddress)
                              .orThrow(new Exception(s"Moving company has rejected the cargo $cargo"))
                              .whenA(isAcceptable)
      } yield ()
    }
  }
}

trait MovingCompany[F[_]] {
  def transport(weight: Int, from: Destination, to: Destination): F[Option[Duration]]

}

object MovingCompany extends Logging.Companion[MovingCompany] {
  def make[F[_]: Monad: Log]: MovingCompany[F] = new MovingCompany[F] {
    override def transport(weight: Int, from: Destination, to: Destination): F[Option[Duration]] =
      if (weight > 1000) warn"Weight $weight is too heavy for the truck".as(None)
      else info"Computing the amount of time it would take to transport from $from to $to...".as(Some(2.days))
  }
}

trait Warehouse[F[_]] {
  def requestStorage(cargo: Cargo): F[Boolean]
  def address: F[Destination]
}

object Warehouse extends Logging.Companion[Warehouse] {
  def makeSmall[F[_]: Monad: Log]: Warehouse[F] = new Warehouse[F] {
    override def requestStorage(cargo: Cargo): F[Boolean] = cargo match {
      case Cargo(weight, name) if weight < 100 => info"Cargo $name is acceptable".as(true)
      case Cargo(weight, name)                 => warn"Cargo $name is too heavy with weight $weight".as(false)
    }

    override def address: F[Destination] = Destination("Smallish str. building 4", "Smallfurkt").pure[F]
  }
}

object Endpoints {

  @derive(decoder)
  case class MoveRequest(cargo: Cargo, from: Destination, to: Destination)
  @derive(decoder)
  case class DepositRequest(cargo: Cargo, daysAmount: Int)

  def cargoEndpoints[I[_]: Sync, F[_]](
      cargoService: CargoService[F]
  )(implicit canRun: WithProvide[F, I, Trace]): HttpRoutes[I] = {
    val dsl = Http4sDsl[I]
    import dsl._

    HttpRoutes.of[I] {
      case request @ POST -> Root / "cargo" / "move" =>
        for {
          MoveRequest(cargo, from, to) <- request.as[MoveRequest]
          trace                         = Trace(32440900L, "move")
          resultComputation             = cargoService.move(cargo, from, to)
          result                       <- runContext[F](resultComputation)(trace)
          response                     <- Ok(s"Moving will take $result")
        } yield response

      case request @ POST -> Root / "cargo" / "deposit" =>
        for {
          DepositRequest(cargo, days) <- request.as[DepositRequest]
          trace                        = Trace(32440900L, "deposit")
          resultComputation            = cargoService.deposit(cargo, days.days)
          _                           <- runContext[F](resultComputation)(trace)
          response                    <- Ok("Success")
        } yield response
    }
  }
}
@derive(loggable)
case class Trace(id: Long, method: String)

object CargoApp extends IOApp {
  import scala.concurrent.ExecutionContext.global
  type TracedIO[A] = ReaderT[IO, Trace, A]

  implicit val logMakeTraced: Logging.Make[TracedIO] = Logging.Make.contextual[TracedIO, Trace]
  implicit val logMake: Logging.Make[IO]             = Logging.Make.plain[IO]
  implicit val appLogger                             = logMake.byName("CargoApp")

  val endpoints = Endpoints.cargoEndpoints[IO, TracedIO](
    CargoService.make(MovingCompany.make[TracedIO], Warehouse.makeSmall[TracedIO])
  )

  val httpApp = Router("/" -> endpoints).orNotFound
  val server  = BlazeServerBuilder[IO](global).bindHttp(8080, "localhost").withHttpApp(httpApp).resource

  def run(args: List[String]): IO[ExitCode] =
    server.use(_ => IO.never).handleErrorWith(error => error"Got $error").as(ExitCode.Success)
}
