package tofu.logging.location

import cats.effect.IO
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import tofu.syntax.location.logging._
class LocationLoggingSpec extends AnyFunSpec with Matchers {
  val locationLogging = LocationLogging.sync[IO, IO]

  it("should log something, but for now I check it just by looking into console") {
    (for {
      implicit0(log: LocationLogging[IO]) <- locationLogging
      _                                   <- info"Whoooo I am locationed"
    } yield ()).unsafeRunSync()
  }
}
