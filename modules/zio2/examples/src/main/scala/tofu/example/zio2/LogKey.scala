package tofu.example.zio2

import tofu.logging.LogAnnotation
import tofu.logging.LogAnnotation.make
import derevo.derive
import tofu.logging.derivation.{hidden, loggable}

import java.util.UUID

@derive(loggable)
case class TestUser(
    login: String,
    @hidden
    password: String,
    accessLevel: Int = 1
)

object LogKey {
  val user: LogAnnotation[TestUser]  = make[TestUser]("user")
  val count: LogAnnotation[Int]      = make[Int]("count")
  val requestId: LogAnnotation[UUID] = make[UUID]("requestId")
}
