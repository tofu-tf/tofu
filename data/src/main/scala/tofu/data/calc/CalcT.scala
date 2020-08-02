package tofu.data
package calc

object CalcT {
  def lift[F[+_], S, A](fa: F[A]): CalcT[F, Any, S, S, Nothing, A] = CalcM.lift[UnaryM[F, +*, +*], S, Nothing, A](fa)
}
