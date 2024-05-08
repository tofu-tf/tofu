package tofu.common.derived

import cats.Eval
import magnolia1.*
import tofu.common.Display
import tofu.magnolia.compat
import scala.deriving.Mirror


trait DisplayDerivationImpl extends AutoDerivation[Display] {

  private type Typeclass[T] = Display[T]

  def join[T](ctx: CaseClass[Typeclass, T]): Display[T]    = (cfg: Display.Config, a: T) => {
    import cfg.*

    val nestedIndent = indent + indent

    def adaptDisplayedParameter(label: String, displayedParameterValue: Vector[String]): Vector[String] = {
      displayedParameterValue match {
        case value +: Vector()              =>
          Vector(indent + label + value)
        case typeHeader +: innerValueParams =>
          val labeledTypeHeader =
            indent + label + typeHeader
          labeledTypeHeader +: innerValueParams // .map(indent + _)
        case _                              => Vector(indent + label)
      }
    }

    val shortName: String = ctx.typeInfo.short

    ctx.parameters.zipWithIndex
      .foldLeft(
        Eval.now(
          Vector(
            s"$shortName ${brackets.left}$newline",
          )
        )
      ) { case (acc, (current, index)) =>
        for {
          alreadyDisplayed                 <- acc
          nestedCfg                         = cfg.copy(indent = nestedIndent, brackets = brackets.copy(right = indent + brackets.right))
          label                             = if (cfg.showFieldLabels) current.label + fieldAssign else ""
          displayedParameterValue          <- current.typeclass.displayBuild(nestedCfg, compat.deref[Typeclass, T](current)(a))
          // this value has at least one element in it by construction,
          // but we avoid using NEVector here due to performance and simplicity
          adapted :+ value                  = adaptDisplayedParameter(label, displayedParameterValue)
          separator                         = if (index + 1 < ctx.parameters.size) fieldSeparator else ""
          adaptedIndentedValueWithSeparator = value + separator + newline
          separatedLabelValue               = adapted :+ adaptedIndentedValueWithSeparator
        } yield alreadyDisplayed ++: separatedLabelValue
      }
      .map(s => s :+ brackets.right)
  }
  def split[T](ctx: SealedTrait[Typeclass, T]): Display[T] = (cfg: Display.Config, a: T) =>
    ctx.choose(a)(adtCase => adtCase.typeclass.displayBuild(cfg, adtCase.cast(a)))

  inline def instance[T](using Mirror.Of[T]): Display[T] = autoDerived[T]

}
