package tofu.logging.location

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox

trait LocationMacroInstances {
  implicit def location: Location = macro LocationMacro.getEnclosingPosition
}

object LocationMacro {

  /** Based on Izumi Logstage CodePosition */
  def getEnclosingPosition(c: blackbox.Context): c.Expr[Location] = {
    import c.universe._
    val applicationPointId: c.Expr[String] = getApplicationPointId(c)
    val file                               = c.Expr[String](Literal(Constant(c.enclosingPosition.source.file.name)))
    val line                               = c.Expr[Int](Literal(Constant(c.enclosingPosition.line)))
    reify {
      Location(file.splice, line.splice, applicationPointId.splice)
    }
  }

  def getApplicationPointId(c: blackbox.Context): c.Expr[String] = {
    import c.universe._
    c.Expr[String](Literal(Constant(getApplicationPointIdImpl(c))))
  }

  def getApplicationPointIdImpl(c: blackbox.Context): String = {
    def goodSymbol(s: c.Symbol): Boolean = {
      val name = s.name.toString
      !name.startsWith("$") && !name.startsWith("<")
    }

    @tailrec
    def rec(s: c.Symbol, st: mutable.ArrayBuffer[c.Symbol]): Unit = {
      st.prepend(s)
      s.owner match {
        case c.universe.NoSymbol =>
        case o                   =>
          rec(o, st)

      }
    }

    val st = mutable.ArrayBuffer[c.Symbol]()
    rec(c.internal.enclosingOwner, st)

    st.tail.flatMap {
      case s if s.isPackage   => Some(s.name)
      case s if goodSymbol(s) => Some(s.name)
      case s                  =>
        if (s.isClass) {
          s.asClass.baseClasses.find(goodSymbol).map(_.name)
        } else {
          None
        }
    }
      .map(_.toString.trim)
      .mkString(".")
  }

}
