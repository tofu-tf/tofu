package tofu.example.logging.zio

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
import tofu.logging.zlogs.{TofuLogging, ZLog, ZLogs}
import tofu.logging.zlogs.implicits._
import tofu.syntax.context._
import tofu.syntax.foption._
import tofu.syntax.logging._
import tofu.syntax.monadic._
import zio.UIO
import zio.ULayer
import tofu.syntax.funk._

import scala.concurrent.duration.{Duration, DurationInt}
import zio._
import zio.interop.catz.implicits._
import zio.interop.catz._
import tofu.zioInstances.implicits._

object types {
  type TracedUIO[+A] = URIO[Has[Trace] with TofuLogging, A]
}
import types.TracedUIO

@derive(loggable, decoder)
case class Destination(address: String, city: String)

@derive(loggable, decoder)
case class Cargo(weight: Int, name: String)

trait CargoService  {
  def move(cargo: Cargo, from: Destination, to: Destination): TracedUIO[Duration]
  def deposit(cargo: Cargo, howLong: Duration): TracedUIO[Unit]
}
object CargoService {

  case class Live(movingCompany: MovingCompany, warehouse: Warehouse) extends CargoService {
    private val serviceAddress = Destination("Cargo Street 51", "New Servexico")

    override def move(cargo: Cargo, from: Destination, to: Destination): TracedUIO[Duration] =
      info"Moving cargo $cargo from $from to $to" *>
        movingCompany
          .transport(cargo.weight, from, to)
          .someOrFail(new Exception(s"Moving company has rejected the cargo $cargo"))
          .orDie

    override def deposit(cargo: Cargo, howLong: Duration): TracedUIO[Unit] = for {
      _                <- info"Depositing cargo $cargo"
      isAcceptable     <- warehouse.requestStorage(cargo)
      warehouseAddress <- warehouse.address
      _                <- movingCompany
                            .transport(cargo.weight, serviceAddress, warehouseAddress)
                            .someOrFail(new Exception(s"Moving company has rejected the cargo $cargo"))
                            .orDie
                            .whenA(isAcceptable)
    } yield ()
  }

  val layer: URLayer[Has[Warehouse] with Has[MovingCompany], Has[CargoService]] =
    (Live(_, _)).toLayer

}
trait MovingCompany  {
  def transport(weight: Int, from: Destination, to: Destination): TracedUIO[Option[Duration]]
}
object MovingCompany {

  case class Live() extends MovingCompany {
    override def transport(weight: Int, from: Destination, to: Destination): TracedUIO[Option[Duration]] =
      if (weight > 1000) warn"Weight $weight is too heavy for the truck".as(None)
      else info"Computing the amount of time it would take to transport from $from to $to...".as(Some(2.days))
  }

  val layer: URLayer[Any, Has[MovingCompany]] = Live.toLayer
}
trait Warehouse  {
  def requestStorage(cargo: Cargo): TracedUIO[Boolean]
  def address: TracedUIO[Destination]
}
object Warehouse {

  case class Live() extends Warehouse {
    override def requestStorage(cargo: Cargo): TracedUIO[Boolean] = cargo match {
      case Cargo(weight, name) if weight < 100 => info"Cargo $name is acceptable".as(true)
      case Cargo(weight, name)                 => warn"Cargo $name is too heavy with weight $weight".as(false)
    }

    override def address: TracedUIO[Destination] = URIO(Destination("Smallish str. building 4", "Smallfurkt"))
  }
  val layer: URLayer[Any, Has[Warehouse]] = Live.toLayer
}

object Endpoints {

  @derive(decoder)
  case class MoveRequest(cargo: Cargo, from: Destination, to: Destination)
  @derive(decoder)
  case class DepositRequest(cargo: Cargo, daysAmount: Int)

  def cargoEndpoints(
      cargoService: CargoService
  ): HttpRoutes[RIO[TofuLogging, *]] = {

    val dsl = Http4sDsl[RIO[TofuLogging, *]]
    import dsl._

    HttpRoutes.of[RIO[TofuLogging, *]] {
      case request @ POST -> Root / "cargo" / "move" =>
        for {
          MoveRequest(cargo, from, to) <- request.as[MoveRequest]
          trace                         = ZLayer.succeed(Trace(32440900L, "move"))
          resultComputation             = cargoService.move(cargo, from, to)
          result                       <- resultComputation.provideSomeLayer[TofuLogging](trace)
          response                     <- Ok(s"Moving will take $result")
        } yield response

      case request @ POST -> Root / "cargo" / "deposit" =>
        for {
          DepositRequest(cargo, days) <- request.as[DepositRequest]
          trace                        = ZLayer.succeed(Trace(32440900L, "deposit"))
          resultComputation            = cargoService.deposit(cargo, days.days)
          _                           <- resultComputation.provideSomeLayer[TofuLogging](trace)
          response                    <- Ok("Success")
        } yield response
    }
  }
}
@derive(loggable)
case class Trace(id: Long, method: String)

object CargoApp extends zio.App {

  implicit val logMakeTraced: ZLogs[Trace] = ZLogs.withContext[Trace]
  implicit val logMake: ZLogs[Any]         = ZLogs.uio
  implicit val appLogger                   = ZLogs.uio.byName("CargoApp")
  val server                               =
    for {
      cargoService                          <- ZIO.access[Has[CargoService]](_.get)
      endpoints                              = Endpoints.cargoEndpoints(cargoService)
      app                                    = Router("/" -> endpoints).orNotFound
      implicit0(rt: ConcurrentEffect[Task]) <- ZIO.runtime
      server                                 = BlazeServerBuilder[Task](global).bindHttp(8080, "localhost").withHttpApp(app).resource
    } yield server

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    server
      .provideLayer(Warehouse.layer ++ MovingCompany.layer ++ CargoService.layer)
      .flatMap(_.use(_ => URIO.never))
      .catchAll(error => error"Got $error")
      .as(zio.ExitCode.success)
}
