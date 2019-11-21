package tofu.logging
package derivation

import cats.Show
import magnolia.{CaseClass, Magnolia, SealedTrait, TypeName}
import org.manatki.derevo.Derivation

object loggable extends Derivation[Loggable] {
  def byShow[T](name: String)(implicit show: Show[T]): Loggable[T] =
    Loggable.stringValue.contramap(show.show).named(name)

  type Typeclass[A] = Loggable[A]

  private[this] def calcTypeName(typeName: TypeName, seen: Set[TypeName] = Set()): String =
    if (seen(typeName)) "#"
    else {
      val args = typeName.typeArguments
      val name = typeName.full

      if (args.isEmpty) name
      else args.iterator.map(calcTypeName(_, seen + typeName)).mkString(name + "[", ",", "]")
    }

  def combine[T](caseClass: CaseClass[Typeclass, T]): Loggable[T] =
    new DictLoggable[T] {
      private[this] val doNotShow = caseClass.annotations.contains(hidden())

      override val typeName  = calcTypeName(caseClass.typeName)
      override val shortName = caseClass.typeName.short

      def fields[I, V, R, M](a: T, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
        caseClass.parameters.iterator
          .filter(!_.annotations.contains(hidden()))
          .foldLeft(receiver.noop(input)) { (acc, param) =>
            val p = param.dereference(a)
            param.typeclass match {
              case _ if param.annotations.contains(unembed()) =>
                receiver.combine(acc, param.typeclass.fields(p, input))
              case _ =>
                receiver.combine(acc, param.typeclass.putField(p, param.label, input))
            }
          }

      override def logShow(a: T): String =
        if (doNotShow) ""
        else
          caseClass.parameters.iterator
            .filter(!_.annotations.contains(hidden()))
            .map(p => s"${p.label}=${p.typeclass.logShow(p.dereference(a))}")
            .mkString(caseClass.typeName.short + "{", ",", "}")
    }

  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Loggable[T] = new Typeclass[T] {
    override val typeName = calcTypeName(ctx.typeName)
    override val shortName    = ctx.typeName.short

    def fields[I, V, R, M](a: T, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      ctx.dispatch(a)(sub => sub.typeclass.fields(sub.cast(a), input))

    def putValue[I, V, R, M](a: T, v: V)(implicit r: LogRenderer[I, V, R, M]): M =
      ctx.dispatch(a)(sub => sub.typeclass.putValue(sub.cast(a), v))
    override def putField[I, V, R, M](a: T, name: String, input: I)(implicit receiver: LogRenderer[I, V, R, M]): R =
      ctx.dispatch(a)(sub => sub.typeclass.putField(sub.cast(a), name, input))
    override def logShow(a: T): String =
      ctx.dispatch(a)(sub => sub.typeclass.logShow(sub.cast(a)))
  }

  def instance[A]: Loggable[A] = macro Magnolia.gen[A]

  implicit def generate[A]: Loggable[A] = macro Magnolia.gen[A]
}
