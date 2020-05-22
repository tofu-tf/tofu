package tofu.optics
package indexed

trait IPDowncast[+I, -S, +T, +A, -B] extends PDowncast[S,T,A, B]{
  def idowncast(s: S): Option[(I, A)]

  override def downcast(s: S): Option[A] = idowncast(s).map(_._2)
}
