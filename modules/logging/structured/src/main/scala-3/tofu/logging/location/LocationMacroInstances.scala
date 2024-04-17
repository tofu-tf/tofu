package tofu.logging.location

import scala.annotation.tailrec
import scala.collection.mutable
import scala.quoted.{Quotes, Expr}

trait LocationMacroInstances:
  inline given location: Location = ${ LocationMacro.getEnclosingPosition }

object LocationMacro:

  /** Based on Izumi Logstage CodePosition */
  def getEnclosingPosition(using quotes: Quotes): Expr[Location] = {
    import quotes.reflect.Position
    val file             = Expr(Position.ofMacroExpansion.sourceFile.name)
    val line             = Expr(Position.ofMacroExpansion.startLine + 1)
    val applicationPoint = getApplicationPointId()
    '{ new Location($file, $line, $applicationPoint) }
  }

  private def ownershipChain()(using quotes: Quotes): Seq[quotes.reflect.Symbol] = {
    import quotes.reflect.*

    @tailrec
    def extractOwnershipChain(s: Symbol, st: mutable.ArrayBuffer[Symbol]): Unit = {
      st.prepend(s)
      s.maybeOwner match {
        case n if n.isNoSymbol =>
        case o                 =>
          extractOwnershipChain(o, st)

      }
    }

    val st = mutable.ArrayBuffer[Symbol]()
    extractOwnershipChain(Symbol.spliceOwner, st)
    st.toSeq
  }

  private def getApplicationPointId()(using quotes: Quotes): Expr[String] = {
    val st = ownershipChain()

    val applicationId = st.tail.flatMap {
      case s if s.isPackageDef => Some(s.name)
      case s if s.isValDef     => None
      case s if goodSymbol(s)  => Some(s.name)
      case _                   => None
    }
      .map(_.trim)
      .mkString(".")

    Expr(applicationId)
  }

  private def goodSymbol(using qctx: Quotes)(s: qctx.reflect.Symbol): Boolean = {
    val name = s.name
    !name.startsWith("$") && !name.startsWith("<")
  }
