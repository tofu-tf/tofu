package tofu.lift

object UnsafeExecFuture {
  def apply[F[_]](implicit ev: UnsafeExecFuture[F]): UnsafeExecFuture[F] = ev
}
