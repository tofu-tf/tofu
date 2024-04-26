package tofu.logging.zlogs

import tofu.logging.Loggable
import tofu.logging.derivation.loggable

object DerivedData {

  final case class User(name: String)

  object User {
    implicit val userLoggable: Loggable[User] = loggable.instance
  }
}
