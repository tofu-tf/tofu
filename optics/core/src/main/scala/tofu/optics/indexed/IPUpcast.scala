package tofu.optics
package indexed

trait IPUpcast[+I, -S, +T, +A, -B] extends PUpcast[S, T, A, B] {
  def iupcast(b: I => B): T

  override def upcast(b: B): T = iupcast(_ => b)
}
