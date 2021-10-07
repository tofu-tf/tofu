# Logging examples

This package contains several copies of the same web-server. This is a simple web-server for some kind of cargo
transporting company (just to have some domain).

Every request to the server is associated with the trace information. This trace is stored in the context of the
effectual response computation and is logged along with every log message done in that computation.

Each copy represents a way to use `tofu.logging` and can easily be started to check how the log messages are shown. Just
use the `Run` button in your IDE (INTELLIJ Idea or Metals both support it).

File [cargo-api.http](cargo-api.http) contains example requests.

## Examples

- [Simple](simple/SimpleLogs.scala) — create logger as in Slf4j or something alike. Recipe
  is [here](https://docs.tofu.tf/docs/logging/recipes/simple)
- [Service](service/ServiceLogs.scala) — automatically create loggers, which will use the location information. Recipe
  is [here](https://docs.tofu.tf/docs/logging/recipes/service)
- [Auto](auto/AutoLogs.scala) — automatically create log messages, which will log the arguments and the result of every
  method. Recipe is [here](https://docs.tofu.tf/docs/logging/recipes/auto)