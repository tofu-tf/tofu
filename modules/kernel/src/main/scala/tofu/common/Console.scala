package tofu.common

import simulacrum.typeclass
import tofu.higherKind.RepresentableK
import tofu.{Delay, higherKind}

import java.io.{BufferedReader, PrintStream}
import scala.annotation.nowarn
import scala.{Console => ScalaConsole}

@typeclass @nowarn("cat=unused-imports")
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

  implicit def delayInstance[F[_]: Delay]: Console[F] = instance(ScalaConsole.in, ScalaConsole.out, ScalaConsole.err)

  def instance[F[_]](in: BufferedReader, out: PrintStream, err: PrintStream)(implicit FD: Delay[F]): Console[F] =
    new Console[F] {
      def readStrLn: F[String] = FD.delay(in.readLine())

      def putStr(s: String): F[Unit]   = FD.delay(out.print(s))
      def putStrLn(s: String): F[Unit] = FD.delay(out.println(s))

      def putErr(e: String): F[Unit]   = FD.delay(err.print(e))
      def putErrLn(e: String): F[Unit] = FD.delay(err.println(e))
    }
}
