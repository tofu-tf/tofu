package tofu.internal.carriers

import tofu.lift.Unlift

// This is purely workaround for scala 2.12
// Which denies to unfold a macro (and recieve a type error)
// before checking an implicit for eligibility
trait UnliftCarrier2[F[_], G[_]] extends Unlift[F, G]

object UnliftCarrier2 extends UnliftCarrier2Macro

trait UnliftCarrier3[F[_], G[_]] extends Unlift[F, G]

object UnliftCarrier3 extends UnliftCarrier3Macro
