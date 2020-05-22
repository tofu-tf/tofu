package tofu.optics.indexed

import tofu.optics.PUpdate

trait IPUpdate[+I, -S, +T, +A, -B] extends PUpdate[S, T, A, B]{
  def iupdate(s: S, fb: (I, A) => B): T

  override def update(s: S, fb: A => B): T = iupdate(s, (_, a) => fb(a))
}
