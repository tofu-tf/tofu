package tofu.logging.location

import tofu.logging.Loggable

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox
import scala.annotation.nowarn

case class Location(file: String, line: Int, applicationPoint: String)

object Location {
  implicit val loggable: Loggable[Location] = Loggable[String].contramap { case Location(file, line, point) =>
    s"$point@($file:$line)"
  }

  implicit def location: Location = macro LocationMacro.getEnclosingPosition
}

@nowarn("cat=lint-infer-any")
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

    st.tail.map {
      case s if s.isPackage   => s.name
      case s if goodSymbol(s) => s.name
      case s                  =>
        if (s.isClass) {
          s.asClass.baseClasses.find(goodSymbol).map(_.name).getOrElse(s.pos.line)
        } else {
          s.pos.line
        }
    }
      .map(_.toString.trim)
      .mkString(".")
  }

}
