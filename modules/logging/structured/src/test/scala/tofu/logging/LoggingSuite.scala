package tofu.logging

import cats.instances.all._
import io.circe.Json
import org.scalatest.flatspec.AnyFlatSpec
import tofu.data.{Calc, ICalc}
import tofu.logging.LoggingSuite.{Exprs, LogEntry, Pasque, Run}
import tofu.syntax.logRenderer._
import tofu.syntax.loggable._
import tofu.syntax.logging._

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

  it should "add throwable" in {
    assert(
      exprs.logCause.execEmpty(Pasque(42)) === Vector(
        LogEntry(
          Logging.Error,
          "runtime exception",
          Json.obj(
            "stacktrace" -> Json.fromString("java.lang.RuntimeException: ya oshibka\n\tat test1.test2(test3:1337)\n"),
            "chaluta"    -> Json.fromLong(42),
          )
        )
      )
    )
  }

  it should "add throwable and more values" in {
    assert(
      exprs.logCauseWith.execEmpty(Pasque(42)) === Vector(
        LogEntry(
          Logging.Error,
          "runtime exception 2",
          Json.obj(
            "stacktrace" -> Json.fromString("java.lang.RuntimeException: ya oshibka\n\tat test1.test2(test3:1337)\n"),
            "chaluta"    -> Json.fromLong(42),
            "why"        -> Json.fromString("kek")
          )
        )
      )
    )
  }

}

object LoggingSuite {

  val exception = new RuntimeException("ya oshibka")
  exception.setStackTrace(Array(new StackTraceElement("test1", "test2", "test3", 1337)))

  class Exprs[F[_]: ({ type L[x[_]] = ServiceLogging[x, LoggingSuite] })#L] {

    val hello: F[Unit] = warn"hello"

    val myLetro: F[Unit] = debug"letro is ${Letro("chankura")}"

    val myLetroAnd: F[Unit] = debugWith"letro is ${Letro("chankura")}" ("nokoma" -> 4, "jakko" -> "ksa")

    val logCause: F[Unit] = errorCause"runtime exception" (exception)

    val logCauseWith: F[Unit] = errorCauseWith"runtime exception 2" (exception)("why" -> "kek")
  }

  case class LogEntry(level: Logging.Level, message: String, obj: Json)

  type Run[+A] = ICalc[Pasque, Vector[LogEntry], Nothing, A]

  implicit val logs: Logs.Universal[Run] = new Logs.Universal[Run] {
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

  implicit val letroLog: Loggable[Letro]   = new DictLoggable[Letro] {
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
