package tofu.internal.carriers

import tofu.kernel.types.Perform

trait PerformCarrier2[F[_]] extends Perform[F, Throwable]

object PerformCarrier2 extends PerformCarrier2Macro

trait PerformCarrier2Context[F[_]] extends Perform[F, Throwable]

object PerformCarrier2Context extends PerformCarrier2ContextMacro

trait PerformCarrier3[F[_]] extends Perform[F, Throwable]

object PerformCarrier3 extends PerformCarrier3Macro
