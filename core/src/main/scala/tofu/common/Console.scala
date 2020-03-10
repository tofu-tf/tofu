package tofu.common

import java.io.{BufferedReader, PrintStream}

import cats.effect.Sync
import simulacrum.typeclass
import tofu.higherKind
import tofu.higherKind.RepresentableK

import scala.{Console => ScalaConsole}

@typeclass
trait Console[F[_]] {
  def readStrLn: F[String]

  def putStr(s: String): F[Unit]
  def putStrLn(s: String): F[Unit]

  def putErr(err: String): F[Unit]
  def putErrLn(err: String): F[Unit]
}

object Console {
  @deprecated("use tofu.syntax.console", since = "0.7.2")
  def readStrLn[F[_]](implicit F: Console[F]): F[String] = F.readStrLn

  @deprecated("use tofu.syntax.console", since = "0.7.2")
  def putStr[F[_]](s: String)(implicit F: Console[F]): F[Unit] = F.putStr(s)

  @deprecated("use tofu.syntax.console", since = "0.7.2")
  def putStrLn[F[_]](s: String)(implicit F: Console[F]): F[Unit] = F.putStrLn(s)

  @deprecated("use tofu.syntax.console", since = "0.7.2")
  def putErr[F[_]](e: String)(implicit F: Console[F]): F[Unit] = F.putErr(e)

  @deprecated("use tofu.syntax.console", since = "0.7.2")
  def putErrLn[F[_]](e: String)(implicit F: Console[F]): F[Unit] = F.putErrLn(e)

  implicit val representableKInstance: RepresentableK[Console] = higherKind.derived.genRepresentableK[Console]

  implicit def syncInstance[F[_]: Sync]: Console[F] = instance(ScalaConsole.in, ScalaConsole.out, ScalaConsole.err)

  def instance[F[_]: Sync](in: BufferedReader, out: PrintStream, err: PrintStream): Console[F] = new Console[F] {
    def readStrLn: F[String] = Sync[F].delay(in.readLine())

    def putStr(s: String): F[Unit]   = Sync[F].delay(out.print(s))
    def putStrLn(s: String): F[Unit] = Sync[F].delay(out.println(s))

    def putErr(e: String): F[Unit]   = Sync[F].delay(err.print(e))
    def putErrLn(e: String): F[Unit] = Sync[F].delay(err.println(e))
  }
}
