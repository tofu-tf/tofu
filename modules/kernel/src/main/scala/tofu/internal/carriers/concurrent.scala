package tofu.internal.carriers

import tofu.concurrent.{MakeAgent, MakeAtom, MakeQVar, MakeSerialAgent}

trait MkAtomCE2Carrier[I[_], F[_]] extends MakeAtom[I, F]

object MkAtomCE2Carrier extends MkAtomCE2CarrierMacro

trait MkAtomCE3Carrier[I[_], F[_]] extends MakeAtom[I, F]

object MkAtomCE3Carrier extends MkAtomCE3CarrierMacro

trait MkQVarCE2Carrier[I[_], F[_]] extends MakeQVar[I, F]

object MkQVarCE2Carrier extends MkQVarCE2CarrierMacro

trait MkQVarCE3Carrier[I[_], F[_]] extends MakeQVar[I, F]

object MkQVarCE3Carrier extends MkQVarCE3CarrierMacro

trait MkAgentCE2Carrier[I[_], F[_]] extends MakeAgent[I, F]

object MkAgentCE2Carrier extends MkAgentCE2CarrierMacro

trait MkAgentCE3Carrier[I[_], F[_]] extends MakeAgent[I, F]

object MkAgentCE3Carrier extends MkAgentCE3CarrierMacro

trait MkSerialAgentCE2Carrier[I[_], F[_]] extends MakeSerialAgent[I, F]

object MkSerialAgentCE2Carrier extends MkSerialAgentCE2CarrierMacro

trait MkSerialAgentCE3Carrier[I[_], F[_]] extends MakeSerialAgent[I, F]

object MkSerialAgentCE3Carrier extends MkSerialAgentCE3CarrierMacro
