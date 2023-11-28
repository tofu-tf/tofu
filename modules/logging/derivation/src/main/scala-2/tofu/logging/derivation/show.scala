package tofu.logging.derivation

import cats.Show

import magnolia1.{CaseClass, Magnolia, SealedTrait}
import derevo.Derivation
import derevo.NewTypeDerivation

object show extends Derivation[Show] with NewTypeDerivation[Show] {

  /** the type constructor for new [[Show]] instances
    *
    * The first parameter is fixed as `String`, and the second parameter varies generically.
    */
  type Typeclass[T] = Show[T]

  /** creates a new [[Show]] instance by labelling and joining (with `mkString`) the result of showing each parameter,
    * and prefixing it with the class name
    */
  def join[T](ctx: CaseClass[Typeclass, T]): Show[T] = value =>
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      param.typeclass.show(param.dereference(value))
    } else strJoin(ctx.typeName.short, masking.params[Typeclass, T](value, ctx.parameters)(_.show))

  /** choose which typeclass to use based on the subtype of the sealed trait */
  def split[T](ctx: SealedTrait[Typeclass, T]): Show[T] = value =>
    ctx.split(value)(sub => sub.typeclass.show(sub.cast(value)))

  def instance[T]: Show[T] = macro Magnolia.gen[T]

  /** bind the Magnolia macro to this derivation object */
  implicit def generate[T]: Show[T] = macro Magnolia.gen[T]
}
