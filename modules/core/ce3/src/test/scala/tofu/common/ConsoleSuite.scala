package tofu.common

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import org.scalatest.funsuite.AnyFunSuite
import cats.effect.IO
import cats.Monad
import tofu.syntax.monadic._
import cats.effect.unsafe.IORuntime

class ConsoleSuite extends AnyFunSuite {
  implicit val iort: IORuntime = IORuntime.global

  def check[A](inputs: String*)(a: => A): (A, Vector[String]) = {
    val input = new ByteArrayInputStream(
      inputs.mkString("\n").getBytes("UTF-8")
    )

    val output = new ByteArrayOutputStream()

    val res = scala.Console.withIn(input) {
      scala.Console.withOut(output)(a)
    }

    val out = new String(output.toByteArray(), "UTF-8").split("\n").toVector
    (res, out)
  }

  // warning: this requires {{io}} to be supplied by name
  // since we need tofu console instance to capture scala.Console.in and scala.Console.out from locally changed vars
  def checkIO(io: => IO[Any], inputs: String*): Vector[String] = check(inputs: _*)(io.unsafeRunSync())._2

  test("simple output") {
    assert(checkIO(Console[IO].putStrLn("Hello, Fedot")) === Vector("Hello, Fedot"))
  }

  def echoProc[F[_]: Console: Monad] =
    Console[F].readStrLn >>= (
      Console[F].putStrLn(_).replicateM_(2)
    )

  test("double echo output") {
    assert(
      checkIO(
        echoProc[IO],
        "feddot"
      ) === Vector("feddot", "feddot")
    )
  }
}
