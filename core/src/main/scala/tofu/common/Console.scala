package tofu.common

import java.io.{BufferedReader, PrintStream}

import scala.{Console => ScalaConsole}

import cats.effect.Sync
import simulacrum.typeclass
import tofu.higherKind
import tofu.higherKind.RepresentableK

@typeclass
trait Console[F[_]] {
  def readStrLn: F[String]

  def putStr(s: String): F[Unit]
  def putStrLn(s: String): F[Unit]

  def putErr(err: String): F[Unit]
  def putErrLn(err: String): F[Unit]
}

object Console {
  def readStrLn[F[_]: Console]: F[String] = Console[F].readStrLn

  def putStr[F[_]: Console](s: String): F[Unit]   = Console[F].putStr(s)
  def putStrLn[F[_]: Console](s: String): F[Unit] = Console[F].putStrLn(s)

  def putErr[F[_]: Console](e: String): F[Unit]   = Console[F].putErr(e)
  def putErrLn[F[_]: Console](e: String): F[Unit] = Console[F].putErrLn(e)

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
