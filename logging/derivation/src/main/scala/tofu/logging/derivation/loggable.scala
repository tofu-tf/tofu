package tofu.logging
package derivation

import cats.Show
import magnolia.{CaseClass, Magnolia, SealedTrait}
import derevo.Derivation

object loggable extends Derivation[Loggable] {
  type Typeclass[A] = Loggable[A]

  def byShow[T: Show](name: String): Loggable[T] =
    Loggable.stringValue.contramap(Show[T].show).named(name)

  def combine[T](ctx: CaseClass[Typeclass, T]): Loggable[T] = new DictLoggable[T] {
    private[this] val doNotShow = ctx.annotations.contains(hidden())

    override val typeName: String  = calcTypeName(ctx.typeName)
    override val shortName: String = ctx.typeName.short

    def fields[I, V, R, M](a: T, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      ctx.parameters.iterator
        .filter(!_.annotations.contains(hidden()))
        .foldLeft(receiver.noop(input)) { (acc, param) =>
          import param._

          val value = dereference(a)
          typeclass match {
            case _ if annotations.contains(unembed()) =>
              receiver.combine(acc, typeclass.fields(value, input))
            case _                                    =>
              receiver.combine(acc, typeclass.putField(value, label, input))
          }
        }

    def logShow(value: T): String =
      if (doNotShow) ""
      else join(ctx.typeName.short, masking.params[Typeclass, T](value, ctx.parameters)(_.logShow))
  }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Loggable[T] = new Typeclass[T] {
    override val typeName: String  = calcTypeName(ctx.typeName)
    override val shortName: String = ctx.typeName.short

    def fields[I, V, R, M](a: T, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      ctx.dispatch(a)(sub => sub.typeclass.fields(sub.cast(a), input))

    def putValue[I, V, R, M](a: T, v: V)(implicit r: LogRenderer[I, V, R, M]): M =
      ctx.dispatch(a)(sub => sub.typeclass.putValue(sub.cast(a), v))

    def logShow(a: T): String =
      ctx.dispatch(a)(sub => sub.typeclass.logShow(sub.cast(a)))

    override def putField[I, V, R, M](a: T, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      ctx.dispatch(a)(sub => sub.typeclass.putField(sub.cast(a), name, input))
  }

  def instance[A]: Loggable[A] = macro Magnolia.gen[A]

  implicit def generate[A]: Loggable[A] = macro Magnolia.gen[A]
}
