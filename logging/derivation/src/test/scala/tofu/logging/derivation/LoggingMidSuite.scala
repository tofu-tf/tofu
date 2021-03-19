package tofu.logging.derivation

import derevo.derive
import org.scalatest.funsuite.AnyFunSuite

@derive(loggingMidTry)
trait Greeter[A, F[_]] {
  def setName(name: String): F[Unit]
  def hello(): F[String]
}

class LoggingMidSuite extends AnyFunSuite
