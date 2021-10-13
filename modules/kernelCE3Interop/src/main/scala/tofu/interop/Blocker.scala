package tofu.interop

import scala.concurrent.ExecutionContext

final case class Blocker[F[_]](ec: ExecutionContext) extends AnyVal
