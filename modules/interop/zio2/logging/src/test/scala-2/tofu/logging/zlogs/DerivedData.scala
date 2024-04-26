package tofu.logging.zlogs

import derevo.derive
import tofu.logging.derivation.loggable

object DerivedData {

  @derive(loggable)
  final case class User(name: String)
}
