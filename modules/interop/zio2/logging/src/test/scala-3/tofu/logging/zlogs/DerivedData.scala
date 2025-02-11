package tofu.logging.zlogs

import tofu.logging.Loggable
import tofu.logging.derivation.loggable
import tofu.logging.derivation._

object DerivedData:
  final case class User(name: String) derives Loggable
