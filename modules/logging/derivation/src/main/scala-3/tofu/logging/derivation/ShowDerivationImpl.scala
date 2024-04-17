package tofu.logging
package derivation

import cats.Show
import magnolia1.*

trait ShowDerivationImpl extends AutoDerivation[Show] {

  def join[T](ctx: CaseClass[Typeclass, T]): Show[T] = value =>
    if (ctx.isValueClass) {
      val param = ctx.parameters.head
      param.typeclass.show(param.deref(value))
    } else strJoin(ctx.typeInfo.short, masking.params[Typeclass, T](value, ctx.parameters)(_.show))

  def split[T](ctx: SealedTrait[Typeclass, T]): Show[T] = value =>
    ctx.choose(value)(sub => sub.typeclass.show(sub.cast(value)))

}
