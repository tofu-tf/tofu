package tofu.data.instances
import cats.Order
import magnolia1.{CaseClass, Magnolia, SealedTrait}

object order {
  type Typeclass[A] = Order[A]

  def join[T](ctx: CaseClass[Order, T]): Order[T] = (x: T, y: T) => {
    val it        = ctx.parameters.iterator
    def go(): Int =
      if (it.hasNext) {
        val p   = it.next()
        val res = p.typeclass.compare(p.dereference(x), p.dereference(y))
        if (res != 0) res
        else go()
      } else 0
    go()
  }

  def split[T](ctx: SealedTrait[Order, T]): Order[T] =
    (x: T, y: T) => {
      val it        = ctx.subtypes.iterator
      def go(): Int =
        if (it.hasNext) {
          val s = it.next()
          if (s.cast.isDefinedAt(x)) {
            if (s.cast.isDefinedAt(y))
              s.typeclass.compare(s.cast(x), s.cast(y))
            else -1
          } else if (s.cast.isDefinedAt(y)) 1
          else go()
        } else throw new Exception("Сould not define subtype")
      go()
    }

  implicit def derive[A]: Order[A] = macro Magnolia.gen[A]
}
