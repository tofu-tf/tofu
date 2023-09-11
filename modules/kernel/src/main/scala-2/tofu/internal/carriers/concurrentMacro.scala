package tofu.internal.carriers

import tofu.internal.Interop

trait MkAtomCE2CarrierMacro {
  final implicit def interopCE2Carrier[I[_], F[_]]: MkAtomCE2Carrier[I, F] =
    macro Interop.delegate2[MkAtomCE2Carrier[I, F], I, F, { val `tofu.interop.CE2Kernel.atomBySync`: Unit }]
}

trait MkAtomCE3CarrierMacro {
  final implicit def interopCE3Carrier[I[_], F[_]]: MkAtomCE3Carrier[I, F] =
    macro Interop.delegate2[MkAtomCE2Carrier[I, F], I, F, { val `tofu.interop.CE3Kernel.atomBySync`: Unit }]
}

trait MkQVarCE2CarrierMacro {
  final implicit def interopCE2Carrier[I[_], F[_]]: MkQVarCE2Carrier[I, F] =
    macro Interop.delegate2[MkQVarCE2Carrier[I, F], I, F, { val `tofu.interop.CE2Kernel.qvarByConcurrent`: Unit }]
}

trait MkQVarCE3CarrierMacro {
  final implicit def interopCE3Carrier[I[_], F[_]]: MkQVarCE3Carrier[I, F] =
    macro Interop.delegate2[MkQVarCE3Carrier[I, F], I, F, { val `tofu.interop.CE3Kernel.qvarByConcurrent`: Unit }]
}

trait MkAgentCE2CarrierMacro {
  final implicit def interopCE2Carrier[I[_], F[_]]: MkAgentCE2Carrier[I, F] =
    macro Interop
      .delegate2[MkAgentCE2Carrier[I, F], I, F, { val `tofu.interop.CE2Kernel.agentByRefAndSemaphore`: Unit }]
}

trait MkAgentCE3CarrierMacro {
  final implicit def interopCE3Carrier[I[_], F[_]]: MkAgentCE3Carrier[I, F] =
    macro Interop
      .delegate2[MkAgentCE3Carrier[I, F], I, F, { val `tofu.interop.CE3Kernel.agentByRefAndSemaphore`: Unit }]
}

trait MkSerialAgentCE2CarrierMacro extends MkSerialAgentCE2Carrier0Macro {
  final implicit def interopCE2Carrier[I[_], F[_]]: MkSerialAgentCE2Carrier[I, F] =
    macro Interop.delegate2[MkSerialAgentCE2Carrier[
      I,
      F
    ], I, F, { val `tofu.interop.CE2Kernel.serialAgentByRefAndSemaphore`: Unit }]
}

trait MkSerialAgentCE2Carrier0Macro {
  final implicit def underlyinginteropCE2Carrier[I[_], F[_]]: MkSerialAgentCE2Carrier[I, F] =
    macro Interop.delegate2[MkSerialAgentCE2Carrier[
      I,
      F
    ], I, F, { val `tofu.interop.CE2Kernel.underlyingSerialAgentByRefAndSemaphore`: Unit }]
}

trait MkSerialAgentCE3CarrierMacro extends MkSerialAgentCE3Carrier0Macro {
  final implicit def interopCE3Carrier[I[_], F[_]]: MkSerialAgentCE3Carrier[I, F] =
    macro Interop.delegate2[MkSerialAgentCE3Carrier[
      I,
      F
    ], I, F, { val `tofu.interop.CE3Kernel.serialAgentByRefAndSemaphore`: Unit }]
}

trait MkSerialAgentCE3Carrier0Macro {
  final implicit def underlyinginteropCE3Carrier[I[_], F[_]]: MkSerialAgentCE3Carrier[I, F] =
    macro Interop.delegate2[MkSerialAgentCE3Carrier[
      I,
      F
    ], I, F, { val `tofu.interop.CE3Kernel.underlyingSerialAgentByRefAndSemaphore`: Unit }]
}