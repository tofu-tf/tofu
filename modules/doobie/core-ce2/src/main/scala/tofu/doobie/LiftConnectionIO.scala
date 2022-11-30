package tofu.doobie

object LiftConnectionIO {
  def apply[DB[_]](implicit ev: LiftConnectionIO[DB]): LiftConnectionIO[DB] = ev
}
