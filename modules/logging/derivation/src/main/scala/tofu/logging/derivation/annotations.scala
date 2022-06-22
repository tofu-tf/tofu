package tofu.logging.derivation

import scala.annotation.StaticAnnotation

/** when applied to the field means "do not log\write this field" when applied to case class means "logShow should be
  * empty string"
  */
final case class hidden() extends StaticAnnotation

/** when applied to the field means "mask field value with given mode" */
final case class masked(mode: MaskMode = MaskMode.Full) extends StaticAnnotation

/** when applied to the field means "log sub fields along with fields of owner" */
final case class unembed() extends StaticAnnotation

final case class ignoreOpt() extends StaticAnnotation
