package tofu.logging.derivation

import scala.annotation.StaticAnnotation

/** When applied to the field it means “do not log\write this field”;
  *
  * When applied to case class it means “logShow should be empty string”.
  */
final case class hidden() extends StaticAnnotation

/** When applied to the field it means “mask field value with given mode” */
final case class masked(mode: MaskMode = MaskMode.Full) extends StaticAnnotation

/** When applied to the field it means “log subfields along with fields of owner” */
final case class unembed() extends StaticAnnotation

/** When applied to the field of type Option[A] it means “do not log\write this field if it is None” */
final case class ignoreOpt() extends StaticAnnotation
