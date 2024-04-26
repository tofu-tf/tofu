package tofu.logging.zlogs
import ZLogAnnotation.make
import tofu.logging.zlogs.DerivedData.User

object LogKey {
  val count: ZLogAnnotation[Int]     = make("count")
  val status: ZLogAnnotation[String] = make("status")
  val user: ZLogAnnotation[User]     = make("user")
}
