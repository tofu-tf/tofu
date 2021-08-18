package tofu

package object concurrent {
  @deprecated("Use Agent.Make", since = "0.11.0")
  type Agents[F[_]]       = MakeAgent[F, F]
  @deprecated("Use SerialAgent.Make", since = "0.11.0")
  type SerialAgents[F[_]] = MakeSerialAgent[F, F]

}
