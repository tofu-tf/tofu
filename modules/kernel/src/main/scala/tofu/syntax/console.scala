package tofu.syntax
import cats.Show
import cats.Show.Shown
import cats.syntax.show._
import tofu.common.Console

object console {
  def readStrLn[F[_]](implicit F: Console[F]): F[String] = F.readStrLn

  def putStr[F[_]](s: String)(implicit F: Console[F]): F[Unit]   = F.putStr(s)
  def putStrLn[F[_]](s: String)(implicit F: Console[F]): F[Unit] = F.putStrLn(s)

  def putErr[F[_]](e: String)(implicit F: Console[F]): F[Unit]   = F.putErr(e)
  def putErrLn[F[_]](e: String)(implicit F: Console[F]): F[Unit] = F.putErrLn(e)

  def putToStringLn[F[_]: Console](a: Any): F[Unit]                   = putStrLn(a.toString)
  def putShowLn[F[_]: Console, A](a: A)(implicit A: Show[A]): F[Unit] = putStrLn(A.show(a))

  final implicit class ConsoleShowExtension(private val ctx: StringContext) extends AnyVal {
    def puts[F[_]: Console](xs: Shown*): F[Unit] = putStrLn(ctx.show(xs: _*))
  }
}
