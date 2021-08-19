package tofu.example.logging


import cats.data.ReaderT
import cats.effect._
import org.http4s._
import org.http4s.dsl.io._

import java.time.LocalDateTime

case class HttpTrace(id: Long, method: String)
case class Cargo(weight: Int, destination: String)
object types {
  type TracedIO[+A] = ReaderT[IO, HttpTrace, A]
  type ArrivalDate = LocalDateTime
}
import types._


class ShipmentService {
  def handleCargo(data: Cargo): TracedIO[ArrivalDate] =  {

  }
}


object Endpoints {
  val helloWorldService(dataService: DataService): HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "business" / data => for {

    } yield ()

  }
}