package tofu.optics
package indexed

trait IPContains[+I, -S, +T, +A, -B]
    extends PContains[S, T, A, B] with IPProperty[I, S, T, A, B] with IPExtract[I, S, T, A, B] {

  def index(s: S): I = iextract(s)._1

  override def extract(s: S): A = iextract(s)._2

  override def inarrow(s: S): Either[T, (I, A)] = Right(iextract(s))
}
