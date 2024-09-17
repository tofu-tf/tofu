package tofu.logging.derivation

import scala.annotation.tailrec

import tofu.magnolia.compat
import tofu.magnolia.compat.Param

object masking {
  private[derivation] def string(shown: String, mode: MaskMode) = {
    @tailrec
    def loop(arr: Array[Char], cur: Int, left: Int): String = {
      if (left == 0 || cur == arr.length) new String(arr)
      else {
        val char = arr(cur)
        if (char.isDigit) arr(cur) = '#'
        else if (char.isLetter) arr(cur) = '*'

        loop(arr, cur + 1, left - 1)
      }
    }

    mode match {
      case MaskMode.Erase                        =>
        "..."
      case MaskMode.Full                         =>
        loop(shown.toCharArray, 0, shown.length)
      case MaskMode.Regexp(pattern)              =>
        pattern
          .findFirstMatchIn(shown)
          .collect {
            case m if m.groupCount == 1 =>
              val start = m.start(1)
              val end   = m.end(1)

              loop(shown.toCharArray, start, end - start)
          }
          .getOrElse(shown)
      case MaskMode.ForLength(offset, maxLength) =>
        loop(shown.toCharArray, shown.length min (offset max 0), if (maxLength == -1) shown.length else maxLength)
      case MaskMode.Custom(f)                    =>
        f(shown)
    }
  }

  private[derivation] def field[T](field: T, shown: String, mode: MaskMode) = field match {
    case None    => shown
    case Some(f) => s"Some(${string(f.toString, mode)})"
    case _       => string(shown, mode)
  }

  private[derivation] def params[TypeClass[_], Type](
      tpe: Type,
      params: Seq[Param[TypeClass, Type]]
  )(fn: TypeClass[Any] => Any => String) =
    params.iterator
      .filterNot(_.annotations.contains(hidden()))
      .flatMap { param =>
        import param._

        val value: PType = compat.deref[TypeClass, Type](param)(tpe)
        if (value == None && param.annotations.contains(ignoreOpt()))
          None
        else {
          val shown = fn(typeclass.asInstanceOf[TypeClass[Any]])(value)

          val repr = annotations.collectFirst { case masked(mode) => field(value, shown, mode) }
            .getOrElse(shown)

          Some(s"$label=$repr")
        }
      }

  implicit final class Ops(private val value: String) extends AnyVal {
    def mask: String = mask(MaskMode.Full)

    def mask(mode: MaskMode): String = string(value, mode)
  }
}
