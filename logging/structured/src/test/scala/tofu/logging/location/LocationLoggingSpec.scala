package tofu.logging.location

import cats.effect.IO
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import tofu.logging.{Logging, Logs}
class LocationLoggingSpec extends AnyFunSpec with Matchers {
  val logs = Logs.sync[IO, IO]

  it("should log something with location from macro, but for now I check it just by looking into console") {
    import tofu.syntax.location.logging._

    (for {
      implicit0(log: Logging[IO]) <- logs.byName("location")
      _                                   <- info"Whoooo I am locationed"
    } yield ()).unsafeRunSync()
  }

  it("should log something without location with name, but for now I check it just by looking into console") {
    import tofu.syntax.logging._

    (for {
      implicit0(log: Logging[IO]) <- logs.byName("non location")
      _                                   <- info"Whoooo I am NON locationed"
    } yield ()).unsafeRunSync()
  }
}
