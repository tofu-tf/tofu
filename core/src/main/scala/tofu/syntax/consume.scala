package tofu.syntax

import cats.{MonoidK, SemigroupK}
import tofu.control.{Consume, Partial, Switch}

object consume
    extends Consume.ToConsumeOps with Partial.ToPartialOps with Switch.ToSwitchOps with MonoidK.ToMonoidKOps
    with SemigroupK.ToSemigroupKOps
