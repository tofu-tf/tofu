package tofu.optics.indexed

trait IPExtract[+I, -S, +T, +A, -B] {
  def iextract(s: S): (I, A)
}
