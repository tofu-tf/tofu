package tofu.internal.carriers

import tofu.internal.Interop
import tofu.lift.Unlift

// This is purely workaround for scala 2.12
// Which denies to unfold a macro (and recieve a type error)
// before checking an implicit for eligibility
class UnliftEffect[F[_], G[_]](val value: Unlift[F, G]) extends AnyVal

object UnliftEffect {
  final implicit def unliftIOEffect[F[_], G[_]]: UnliftEffect[F, G] =
    macro Interop.delegate[UnliftEffect[F, G], G, { val `tofu.interop.CE2Kernel.unliftEffect`: Unit }]
}
