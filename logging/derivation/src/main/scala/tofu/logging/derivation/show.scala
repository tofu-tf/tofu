package tofu.logging.derivation

import cats.Show
import magnolia.{CaseClass, Magnolia, SealedTrait}
import org.manatki.derevo.Derivation

object show extends Derivation[Show] {

  /** the type constructor for new [[Show]] instances
    *
    *  The first parameter is fixed as `String`, and the second parameter varies generically. */
  type Typeclass[T] = Show[T]

  def join(typeName: String, strings: TraversableOnce[String]): String =
    if (strings.isEmpty) typeName else strings.mkString(s"$typeName{", ",", "}")

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`) the result of
    *  showing each parameter, and prefixing it with the class name */
  def combine[T](ctx: CaseClass[Typeclass, T]): Show[T] = { value =>
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      param.typeclass.show(param.dereference(value))
    } else {
      val paramStrings = ctx.parameters.iterator.filter(param => !param.annotations.contains(hidden())).map { param =>
        val pvalue: param.PType = param.dereference(value)
        val shown               = param.typeclass.show(pvalue)
        val repr = param.annotations.collectFirst {
          case masked(erase) => maskField(pvalue, shown, erase)
        }.getOrElse(shown)
        s"${param.label}=$repr"
      }

      join(ctx.typeName.short, paramStrings)
    }
  }

  /** choose which typeclass to use based on the subtype of the sealed trait */
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Show[T] =
    (value: T) =>
      ctx.dispatch(value) { sub =>
        sub.typeclass.show(sub.cast(value))
    }

  /** bind the Magnolia macro to this derivation object */
  implicit def genShow[T]: Show[T] = macro Magnolia.gen[T]
  implicit def instance[T]: Show[T] = macro Magnolia.gen[T]

  private def maskString(shown: String, erase: Boolean): String =
    if (erase) "..."
    else
      shown.map {
        case c if c.isDigit  => '#'
        case c if c.isLetter => '*'
        case c               => c
      }
  private val SomeRe = "(?<=^Some(\\{value=|\\())(.+)(?=(\\}|\\))$)".r

  private def maskField[T](field: T, shown: String, erase: Boolean): String = field match {
    case None    => shown
    case Some(_) => SomeRe.replaceSomeIn(shown, m => Some(maskString(m.toString, erase)))
    case _       => maskString(shown, erase)
  }

  implicit final class MaskingOps(private val value: String) extends AnyVal {
    def mask: String = maskString(value, erase = false)
  }
}