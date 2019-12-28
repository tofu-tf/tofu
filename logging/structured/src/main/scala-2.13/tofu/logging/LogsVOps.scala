package tofu.logging

trait LogsVOps[I[_], F[_]] { self: Logs[I, F] =>
  final def named[name <: String with Singleton](
      name: ValueOf[name]
  ): I[ServiceLogging[F, name]] =
    byName(name.value).asInstanceOf[I[ServiceLogging[F, name]]]

}
