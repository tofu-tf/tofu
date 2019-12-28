package tofu.common
import cats.effect.Sync
import tofu.higherKind
import tofu.higherKind.RepresentableK

import scala.io.StdIn

trait Console[F[_]] {
  def putStr(s: String): F[Unit]
  def putStrLn(s: String): F[Unit]

  def readStrLn: F[String]
}

object Console {
  def putStr[F[_]](s: String)(implicit C: Console[F]): F[Unit]   = C.putStr(s)
  def putStrLn[F[_]](s: String)(implicit C: Console[F]): F[Unit] = C.putStrLn(s)
  def readStrLn[F[_]](implicit C: Console[F]): F[String]         = C.readStrLn

  implicit val representableKInstance: RepresentableK[Console] = higherKind.derived.genRepresentableK[Console]

  implicit def syncInstance[F[_]](implicit F: Sync[F]): Console[F] = new Console[F] {
    def putStr(s: String): F[Unit]   = F.delay(print(s))
    def putStrLn(s: String): F[Unit] = F.delay(println(s))
    def readStrLn: F[String]         = F.delay(StdIn.readLine())
  }
}
