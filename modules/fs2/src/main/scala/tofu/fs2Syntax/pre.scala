package tofu.fs2Syntax

import cats.tagless.ApplyK
import fs2.Stream
import tofu.fs2.Fs2Syntax
import tofu.higherKind.Pre.T
import tofu.syntax.funk.funK

@deprecated("Use tofu.fs2.syntax or tofu.fs2.implicits instead", "0.11.0")
object pre extends Fs2Syntax
