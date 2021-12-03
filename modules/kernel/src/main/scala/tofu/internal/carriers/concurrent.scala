package tofu.internal.carriers

import tofu.concurrent.{MakeAgent, MakeAtom, MakeQVar, MakeSerialAgent}
import tofu.internal.Interop

trait MkAtomCE2Carrier[I[_], F[_]] extends MakeAtom[I, F]

object MkAtomCE2Carrier {
  final implicit def interopCE2Carrier[I[_], F[_]]: MkAtomCE2Carrier[I, F] =
    macro Interop.delegate2[MkAtomCE2Carrier[I, F], I, F, { val `tofu.interop.CE2Kernel.atomBySync`: Unit }]
}

trait MkAtomCE3Carrier[I[_], F[_]] extends MakeAtom[I, F]

object MkAtomCE3Carrier {
  final implicit def interopCE3Carrier[I[_], F[_]]: MkAtomCE3Carrier[I, F] =
    macro Interop.delegate2[MkAtomCE2Carrier[I, F], I, F, { val `tofu.interop.CE3Kernel.atomBySync`: Unit }]
}

trait MkQVarCE2Carrier[I[_], F[_]] extends MakeQVar[I, F]

object MkQVarCE2Carrier {
  final implicit def interopCE2Carrier[I[_], F[_]]: MkQVarCE2Carrier[I, F] =
    macro Interop.delegate2[MkQVarCE2Carrier[I, F], I, F, { val `tofu.interop.CE2Kernel.qvarByConcurrent`: Unit }]
}

trait MkQVarCE3Carrier[I[_], F[_]] extends MakeQVar[I, F]

object MkQVarCE3Carrier {
  final implicit def interopCE3Carrier[I[_], F[_]]: MkQVarCE3Carrier[I, F] =
    macro Interop.delegate2[MkQVarCE3Carrier[I, F], I, F, { val `tofu.interop.CE3Kernel.qvarByConcurrent`: Unit }]
}

trait MkAgentCE2Carrier[I[_], F[_]] extends MakeAgent[I, F]

object MkAgentCE2Carrier {
  final implicit def interopCE2Carrier[I[_], F[_]]: MkAgentCE2Carrier[I, F] =
    macro Interop
      .delegate2[MkAgentCE2Carrier[I, F], I, F, { val `tofu.interop.CE2Kernel.agentByRefAndSemaphore`: Unit }]
}

trait MkAgentCE3Carrier[I[_], F[_]] extends MakeAgent[I, F]

object MkAgentCE3Carrier {
  final implicit def interopCE3Carrier[I[_], F[_]]: MkAgentCE3Carrier[I, F] =
    macro Interop
      .delegate2[MkAgentCE3Carrier[I, F], I, F, { val `tofu.interop.CE3Kernel.agentByRefAndSemaphore`: Unit }]
}

trait MkSerialAgentCE2Carrier[I[_], F[_]] extends MakeSerialAgent[I, F]

object MkSerialAgentCE2Carrier extends MkSerialAgentCE2Carrier0 {
  final implicit def interopCE2Carrier[I[_], F[_]]: MkSerialAgentCE2Carrier[I, F] =
    macro Interop.delegate2[MkSerialAgentCE2Carrier[
      I,
      F
    ], I, F, { val `tofu.interop.CE2Kernel.serialAgentByRefAndSemaphore`: Unit }]
}

trait MkSerialAgentCE2Carrier0 {
  final implicit def underlyinginteropCE2Carrier[I[_], F[_]]: MkSerialAgentCE2Carrier[I, F] =
    macro Interop.delegate2[MkSerialAgentCE2Carrier[
      I,
      F
    ], I, F, { val `tofu.interop.CE2Kernel.underlyingSerialAgentByRefAndSemaphore`: Unit }]

}

trait MkSerialAgentCE3Carrier[I[_], F[_]] extends MakeSerialAgent[I, F]

object MkSerialAgentCE3Carrier extends MkSerialAgentCE3Carrier0 {
  final implicit def interopCE3Carrier[I[_], F[_]]: MkSerialAgentCE3Carrier[I, F] =
    macro Interop.delegate2[MkSerialAgentCE3Carrier[
      I,
      F
    ], I, F, { val `tofu.interop.CE3Kernel.serialAgentByRefAndSemaphore`: Unit }]
}
trait MkSerialAgentCE3Carrier0 {
  final implicit def underlyinginteropCE3Carrier[I[_], F[_]]: MkSerialAgentCE3Carrier[I, F] =
    macro Interop.delegate2[MkSerialAgentCE3Carrier[
      I,
      F
    ], I, F, { val `tofu.interop.CE3Kernel.underlyingSerialAgentByRefAndSemaphore`: Unit }]

}
