package tofu.example.zio2

import derevo.derive
import tofu.logging.TofuAnnotation
import tofu.logging.TofuAnnotation.make
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

  val user: TofuAnnotation[TestUser]  = make[TestUser]("user")
  val count: TofuAnnotation[Int]      = make[Int]("count")
  val requestId: TofuAnnotation[UUID] = make[UUID]("requestId")
}
