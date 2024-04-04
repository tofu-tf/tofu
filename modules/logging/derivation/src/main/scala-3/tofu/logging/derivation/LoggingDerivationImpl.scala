package tofu.logging
package derivation

import cats.Show
import magnolia1.*
import scala.deriving.Mirror

trait LoggingDerivationImpl extends AutoDerivation[Loggable] {

  def byShow[T: Show](name: String): Loggable[T] =
    Loggable.stringValue.contramap(Show[T].show).named(name)

  def join[T](ctx: CaseClass[Typeclass, T]): Loggable[T] = new DictLoggable[T] {
    private[this] val doNotShow = ctx.annotations.contains(hidden())

    override val typeName: String  = calcTypeName(ctx.typeInfo)
    override val shortName: String = ctx.typeInfo.short

    def fields[I, V, R, M](a: T, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      ctx.parameters.iterator
        .filter(!_.annotations.contains(hidden()))
        .foldLeft(receiver.noop(input)) { (acc, param) =>
          import param._

          val value = deref(a)
          typeclass match {
            case _ if annotations.contains(unembed()) =>
              receiver.combine(acc, typeclass.fields(value, input))
            case _                                    =>
              annotations.collectFirst { case masked(mode) =>
                receiver.combine(acc, typeclass.putMaskedField(value, label, input)(masking.string(_, mode)))
              }.getOrElse(receiver.combine(acc, typeclass.putField(value, label, input)))
          }
        }

    def logShow(value: T): String =
      if (doNotShow) ""
      else strJoin(ctx.typeInfo.short, masking.params[Typeclass, T](value, ctx.parameters)(_.logShow))
  }

  def split[T](ctx: SealedTrait[Typeclass, T]): Loggable[T] = new Typeclass[T] {
    override val typeName: String  = calcTypeName(ctx.typeInfo)
    override val shortName: String = ctx.typeInfo.short

    def fields[I, V, R, M](a: T, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      ctx.choose(a)(sub => sub.typeclass.fields(sub.cast(a), input))

    def putValue[I, V, R, M](a: T, v: V)(implicit r: LogRenderer[I, V, R, M]): M =
      ctx.choose(a)(sub => sub.typeclass.putValue(sub.cast(a), v))

    def logShow(a: T): String =
      ctx.choose(a)(sub => sub.typeclass.logShow(sub.cast(a)))

    override def putField[I, V, R, M](a: T, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      ctx.choose(a)(sub => sub.typeclass.putField(sub.cast(a), name, input))
  }

  inline def instance[A](using Mirror.Of[A]): Loggable[A] = autoDerived[A]
}
