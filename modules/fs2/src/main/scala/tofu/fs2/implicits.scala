package tofu.fs2

object implicits extends Fs2Instances1 with Fs2Syntax

object LiftStream {
  def apply[S[_], F[_]](implicit ev: LiftStream[S, F]): LiftStream[S, F] = ev
}
