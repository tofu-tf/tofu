package tofu.logging
package derivation

import derevo.derive
import org.scalatest.funsuite.AnyFunSuite
import org.slf4j.helpers.MessageFormatter
import tofu.data.*
import glass.macros.Optics
import tofu.higherKind.RepK
import tofu.higherKind.RepresentableK
import cats.~>

import LoggingMidSuite.*

@derive(loggingMidTry)
trait Greeter[F[_]] {
  def setName(name: String): F[Unit]
  def hello: F[String]
}

object Greeter extends LoggingCompanion[Greeter] {
  import LoggingMidSuite.*

  implicit val repK: RepresentableK[Greeter] =
    new RepresentableK[Greeter] {
      def tabulate[F[_]](hom: RepK[Greeter, _] ~> F): Greeter[F] = new Greeter[F] {
        def setName(name: String): F[Unit] = hom(RepK[Greeter](_.setName(name)))
        def hello: F[String]               = hom(RepK[Greeter](_.hello))
      }
    }
  implicit object Instance extends Greeter[Eff] {
    def setName(name: String): Eff[Unit] =
      CalcM.set(Some(name)).focus(State.name).void

    def hello: Eff[String] = CalcM.get[State].map(_.name).flatMap {
      case None       => CalcM.raise(MissingName())
      case Some(name) => CalcM.pure(s"Hello, $name")
    }
  }
}

object LoggingMidSuite {

  case class MissingName() extends Throwable

  @Optics
  case class State(name: Option[String] = None, logs: Vector[String] = Vector())

  type Eff[+A] = ICalcM[Nothing2T, Any, State, Throwable, A]

  def logging(name: String): Logging[Eff] = new Logging[Eff] {
    private def put(message: String)(logs: Vector[String])                            = logs :+ message
    def write(level: Logging.Level, message: String, values: LoggedValue*): Eff[Unit] = {
      val interpolated = MessageFormatter.arrayFormat(message, values.toArray).getMessage
      CalcM.update(put(s"[$level] <$name> $interpolated")).void.focus(State.logs)
    }
  }

  implicit val logs: Logging.Make[Eff] = logging(_)
}

class LoggingMidSuite extends AnyFunSuite {
  val greeter: Greeter[Eff] = Greeter.Instance.attachErrLogs[Throwable]
  val ErrName               = classOf[MissingName].getName
  val GreeterName           = classOf[Greeter[Any]].getName

  test("should raise an error when instance is not set") {
    val (State(_, logs), result) = greeter.hello.runUnit(State())
    assert(result === Left(MissingName()))
    assert(
      logs === Vector(
        s"[Debug] <$GreeterName> entering hello ()",
        s"[Error] <$GreeterName> error during hello () error is $ErrName"
      )
    )
  }

  test("should succesfully read name with name is set") {
    val (State(_, logs), result) = (greeter.setName("Tofu") >> greeter.hello).runUnit(State())
    assert(result === Right("Hello, Tofu"))

    assert(
      logs === Vector(
        s"[Debug] <$GreeterName> entering setName (name = Tofu)",
        s"[Debug] <$GreeterName> leaving setName (name = Tofu) with result ()",
        s"[Debug] <$GreeterName> entering hello ()",
        s"[Debug] <$GreeterName> leaving hello () with result Hello, Tofu"
      )
    )
  }
}
