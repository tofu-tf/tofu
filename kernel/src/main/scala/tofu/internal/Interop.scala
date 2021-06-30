package tofu.internal
import scala.reflect.macros.blackbox._

class Interop(val c: Context) {
  import c.universe._
  def delegate[R, F[_], N](implicit R: c.WeakTypeTag[R], FU: c.WeakTypeTag[F[Unit]], N: c.WeakTypeTag[N]): c.Expr[R] = {
    val s   = N.tpe.decls.head.asTerm.name.decodedName.toString()
    val exp = c.parse(s)
    val F   = FU.tpe.typeConstructor

    c.Expr[R](q"$exp[$F]")
  }
}
