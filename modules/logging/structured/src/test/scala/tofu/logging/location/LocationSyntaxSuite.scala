package tofu.logging.location

import cats.Id
import cats.data.Writer
import cats.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import tofu.logging.{LoggedValue, Logging, Logs}
import tofu.syntax.location.logging._

object WriterLogs extends Logs[Id, Writer[List[String], _]] {
  def byName(name: String): Id[Logging[Writer[List[String], _]]] = new Logging[Writer[List[String], _]] {
    override def write(level: Logging.Level, message: String, values: LoggedValue*): Writer[List[String], Unit] =
      Writer.tell(s"${level.toString.toUpperCase} | $message" :: values.map(_.toString).toList)
  }
}

class TestService {
  type Run[A] = Writer[List[String], A]
  implicit val logger: Logging[Writer[List[String], _]] = WriterLogs.byName("TestLogger")

  def sayHello(name: String): Run[String] =
    for {
      _    <- trace"I am about to say hello to $name"
      hello = s"Hello, my dear $name"
      _    <- info"Yes, I did it, I said hello!"
    } yield hello

  def sayGoodbye(name: String): Run[String] =
    for {
      _      <- error"I don't want to say goodbye to $name"
      goodbye = s"Goodbye, my sweet $name"
      _      <- info"Let the stars shine through"
    } yield goodbye
}

class LocationSyntaxSuite extends AnyFlatSpec {
  "tofu.syntax.location.logging" should "add location as additional logged value" in {
    val testService   = new TestService
    val (log, result) = (for {
      hello   <- testService.sayHello("Бублик the good dog")
      goodbye <- testService.sayGoodbye("Бублик")
    } yield s"$hello; $goodbye").run

    assertResult("Hello, my dear Бублик the good dog; Goodbye, my sweet Бублик")(result)
    assert(
      List(
        "TRACE | I am about to say hello to {}",
        "Бублик the good dog",
        "tofu.logging.location.TestService.sayHello@(LocationSyntaxSuite.scala:23)",
        "INFO | Yes, I did it, I said hello!",
        "tofu.logging.location.TestService.sayHello@(LocationSyntaxSuite.scala:25)",
        "ERROR | I don't want to say goodbye to {}",
        "Бублик",
        "tofu.logging.location.TestService.sayGoodbye@(LocationSyntaxSuite.scala:30)",
        "INFO | Let the stars shine through",
        "tofu.logging.location.TestService.sayGoodbye@(LocationSyntaxSuite.scala:32)"
      ) == log
    )
  }
}
