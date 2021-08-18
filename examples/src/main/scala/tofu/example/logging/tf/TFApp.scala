package tofu.example.logging.tf

class TFApp {

}


import cats.effect._, org.http4s._, org.http4s.dsl.io._,


object Endpoints {
  val helloWorldService = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
  }
}