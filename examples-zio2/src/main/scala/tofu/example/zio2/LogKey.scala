package tofu.example.zio2

import derevo.derive
import tofu.logging.zlogs.ZLogAnnotation
import tofu.logging.zlogs.ZLogAnnotation.make
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

  val user: ZLogAnnotation[TestUser]  = make[TestUser]("user")
  val count: ZLogAnnotation[Int]      = make[Int]("count")
  val requestId: ZLogAnnotation[UUID] = make[UUID]("requestId")
}
