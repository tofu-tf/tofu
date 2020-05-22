package tofu.logging

import cats.instances.all._
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import tofu.data.{Calc, ICalc}
import tofu.logging.LoggingSuite.{Exprs, LogEntry, Pasque, Run}
import tofu.syntax.logRenderer._
import tofu.syntax.loggable._
import tofu.syntax.logging._

import scala.reflect.{ClassTag, classTag}

class LoggingSuite extends AnyFlatSpec {
  val exprs = new Exprs[Run]

  "log tree" should "render map" in {
    assert(
      LogTree(Map[String, LoggedValue]("koo" -> 1, "lo" -> "foo")) ===
        Json.obj("koo" -> Json.fromInt(1), "lo" -> Json.fromString("foo"))
    )
  }

  "logging syntax" should "save the context" in {
    assert(
      exprs.hello.execEmpty(Pasque(42)) === Vector(
        LogEntry(Logging.Warn, "hello", Json.obj("chaluta" -> Json.fromLong(42)))
      )
    )
  }

  it should "add logged values" in {
    assert(
      exprs.myLetro.execEmpty(Pasque(42)) === Vector(
        LogEntry(
          Logging.Debug,
          "letro is {}",
          Json.obj(
            "chaluta" -> Json.fromLong(42),
            "quasti"  -> Json.fromString("chankura")
          )
        )
      )
    )
  }

  it should "add more values" in {
    assert(
      exprs.myLetroAnd.execEmpty(Pasque(42)) === Vector(
        LogEntry(
          Logging.Debug,
          "letro is {}",
          Json.obj(
            "chaluta" -> Json.fromLong(42),
            "quasti"  -> Json.fromString("chankura"),
            "nokoma"  -> Json.fromLong(4),
            "jakko"   -> Json.fromString("ksa")
          )
        )
      )
    )
  }

}

object LoggingSuite {

  class Exprs[F[_]: ServiceLogging[*[_], LoggingSuite]] {

    val hello: F[Unit] = warn"hello"

    val myLetro: F[Unit] = debug"letro is ${Letro("chankura")}"

    val myLetroAnd: F[Unit] = debugWith"letro is ${Letro("chankura")}" ("nokoma" -> 4, "jakko" -> "ksa")
  }

  case class LogEntry(level: Logging.Level, message: String, obj: Json)

  type Run[+A] = ICalc[Pasque, Vector[LogEntry], Nothing, A]

  implicit val logs: Logs.Universal[Run] = new Logs.Universal[Run] {
    def forService[Svc: ClassTag]: Logging[Run] = byName(classTag[Svc].runtimeClass.getName)

    def byName(name: String): Logging[Run] =
      (level, message, values) =>
        (Calc.read: Run[Pasque]).flatMap { ctx =>
          val json = LogTree.build((ctx.loggedValue +: values): _*)
          Calc.write(Vector(LogEntry(level, message, json)))
        }
  }

  case class Letro(
      quasti: String
  )

  implicit val letroLog: Loggable[Letro] = new DictLoggable[Letro] {
    def fields[I, V, R, S](a: Letro, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      i.addString("quasti", a.quasti)
    def logShow(a: Letro): String                                                  = s"Letro{quasti=${a.quasti}}"
  }

  case class Pasque(
      chaluta: Long
  )
  implicit val pasqueLog: Loggable[Pasque] = new DictLoggable[Pasque] {
    def fields[I, V, R, S](a: Pasque, i: I)(implicit r: LogRenderer[I, V, R, S]): R =
      i.addInt("chaluta", a.chaluta)

    def logShow(a: Pasque): String = s"Pasque{chaluta=${a.chaluta}}"
  }
}
