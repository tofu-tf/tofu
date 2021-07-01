package tofu.internal
import scala.reflect.macros.blackbox._
import scala.annotation.unused

class Interop(val c: Context) {
  import c.universe._
  import c.{WeakTypeTag => WTT}
  private type WTTU[F[_]] = WTT[F[Unit]]

  private def tc[F[_]](implicit wttu: WTTU[F]) = wttu.tpe.typeConstructor
  private def t[A](implicit wttu: WTT[A])      = wttu.tpe

  private def delegateImpl[R, N](ts: Type*)(implicit R: WTT[R], N: WTT[N]): c.Expr[R] = {
    val s   = N.tpe.decls.head.asTerm.name.decodedName.toString
    val exp = c.parse(s)

    c.Expr[R](q"$exp[..$ts]")
  }

  def delegate[R: WTT, F[_]: WTTU, N: WTT]: c.Expr[R] = delegateImpl[R, N](tc[F])

  def delegate1e1[R: WTT, F[_]: WTTU, A: WTT, N: WTT](@unused ev1: Tree): c.Expr[R] = delegateImpl[R, N](tc[F], t[A])
}
