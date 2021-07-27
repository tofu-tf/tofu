package tofu.lift

object UnliftIO {
  def apply[F[_]](implicit ev: UnliftIO[F]): UnliftIO[F] = ev
}
