package tofu

object ErrorsCompanion {
  sealed trait Err
  object Err1 extends Err
  object Err2 extends Err

  object Err extends Errors.Companion[Err]

  def raise[F[_]: Err.Raise]: Err.Raise[F] = Raise[F, Err]

  def handle[F[_]: Err.Handle]: Err.Handle[F] = Handle[F, Err]

  def errors[F[_]: Err.Errors]: Err.Errors[F] = Errors[F, Err]
}
