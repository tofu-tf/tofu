---
id: tofu.logging.recipes.zio2
title: ZIO2 Logging
---

Tofu provides an implementation of `zio.ZLogger` and special annotations called `ZLogAnnotation`
for [ZIO logging facade](https://zio.dev/guides/tutorials/enable-logging-in-a-zio-application).
If you feel more confident with [Tofu Logging](./tofu.logging.main.entities.md#logging) interface, `ULogging`
, `ZLogging.Make`, `ZLogs` are at your service.

First add the following dependency:

```sbt
libraryDependencies += "tf.tofu" %% "tofu-zio2-logging" % "<latest version in the badge in README>"
```

Then import the package:

```scala
import tofu.logging.zlogs._
```

## ZIO 2 logging facade

To use Tofu with ZIO logging facade just add `TofuZLogger` to your app runtime:

```scala
object Main extends ZIOAppDefault {

  val program: UIO[Unit] = ZIO.log("Hello, ZIO logging!")

  override def run = {
    program.logSpan("full_app") @@ ZIOAspect.annotated("foo", "bar")
  }.provide(
    Runtime.removeDefaultLoggers,
    TofuZLogger.addToRuntime
  )

}
```

The log will be:

```json
{
  "level": "INFO",
  "logger_name": "tofu.logging.zlogs.Main",
  "message": "Hello, ZIO logging!",
  "zSpans": {
    "full_app": 440
  },
  "zAnnotations": {
    "foo": "bar"
  }
}
```

* __logger_name__ is parsed from `zio.Trace` which contains the location where log method is called
* all `zio.LogSpan` are collected in the json object named __zSpans__
* all `zio.LogAnnotation` are collected in the json object named __zAnnotations__ (to avoid conflicts with Tofu
  annotations)

### ZLogAnnotation

A specialized version of [LogAnnotation](./tofu.logging.annotation.md) allows you to add a context via ZIO aspects:

```scala
import tofu.logging.zlogs.ZLogAnnotation._

val httpCode: ZLogAnnotation[Int] = make("httpCode")

override def run = {
  program @@ httpCode(204) @@ loggerName("MyLogger")
}.provide(
  Runtime.removeDefaultLoggers,
  TofuZLogger.addToRuntime
)
```

will produce:

```json
{
  "level": "INFO",
  "loggerName": "MyLogger",
  "message": "Hello, ZIO logging!",
  "httpCode": 204
}
```

You can change the logger name via `ZLogAnnotation.loggerName`.

Also you can create scoped annotations via `ZLogAnnotation.scoped`

```scala
import tofu.logging.zlogs.ZLogAnnotation._

val httpCode: ZLogAnnotation[Int] = make("httpCode")

override def run = {
  ZIO.scoped {
    for {
      _    <- httpCode.scoped(code)
      _    <- program
    } yield ()
  }
}.provide(
  Runtime.removeDefaultLoggers,
  TofuZLogger.addToRuntime
)
```

`ZLogAnnotation.make[A]` implicitly requires a `Loggable[A]` instance, see more
in [Loggable](./tofu.logging.loggable.md) section.

### TofuDefaultContext

Using this service you can look up an element from the context added via `ZLogAnnotation`:

```scala
val httpCode: ZLogAnnotation[Int] = make("httpCode")

val program = {
  for {
    maybeCode <- ZIO.serviceWithZIO[TofuDefaultContext](_.getValue(httpCode)) // Some(204)
    //...
  } yield ()
} @@ httpCode(204)
```

## ZIO implementation of Tofu Logging

If you want more flexible Tofu Logging, `tofu-zio2-logging` provides some useful stuff:

* `ULogging` - is a type alias for `Logging[UIO]`, logging methods of this logger look
  like `def info(message: String, values: LoggedValue*): UIO[Unit]`

* Logger factory type aliases:
    - `ZLogging.Make` - is a type alias for `Logs[Id, UIO]`, produces plain instances of `ULogging`.
    - `ULogs` - is a type alias for `Logs[UIO, UIO]`, produces instances of `ULogging` inside `UIO` effect.

* `ZLogging.Make` and `ZLogs` objects provide corresponding factory instances
    - `layerPlain` creates layer contains simple implementation of `ZLogging.Make` (or `ULogs`)
    - `layerPlainWithContext` creates layer with an implementation of `ZLogging.Make` (or `ULogs`) producing loggers
      which add the context to your logs from the `ContextProvider`.
      This one is supposed to be provided at the app creation point via ZLayer-s.
    - `layerContextual[R: Loggable]` makes a factory `ZMake[R]` (or `ZLogs[R]`) of contextual `ZLogging[R]` retrieving a
      context from
      the ZIO environment of the logging methods. This legacy approach is contrary to
      ZIO [Service Pattern](https://zio.dev/reference/service-pattern/), so we won't cover it here.

### ContextProvider

If we need to carry some contextual information and don't want to use the ZIO environment to store it, we can use
a `ContextProvider`:

```scala
trait ContextProvider {
  def getCtx: UIO[LoggedValue]
}
```

This service required by `layerPlainWithContext` factory. Every logger will log the provided `LoggedValue` which
evaluated every time the log method is called. ZIO encourage us to
use [FiberRef](https://zio.dev/reference/state-management/fiberref) under the hood, which binds the context to an
executing fiber.
It can be convenient to use `ValuedContextProvider` to implement your own instance:

```scala
abstract class ValueContextProvider[A](implicit L: Loggable[A]) extends ContextProvider {
  protected def getA: UIO[A]
}
```

Or you can use `TofuDefaultContext` (implements `ContextProvider`) which provides all tofu annotations added
via `ZLogAnnotation`:

* `TofuDefaultContext.layerZioContextOff` — logs just tofu annotations
* `TofuDefaultContext.layerZioContextOn` — includes `LogSpan`-s and zio annotations

## Example

Let's write a simple service which logs a current date.

```scala
import tofu.logging.zlogs._
import zio._

val currentDate: ZLogAnnotation[LocalDate] = make("today")

class FooBarService(logger: ULogging) {
  def doLogs: UIO[Unit] = for {
    now <- Clock.localDateTime
    _ <- logger.info("Got current date {}", currentDate -> now.toLocalDate)
  } yield {}
}

object FooBarService {
  val layer: URLayer[ULogs, FooBarService] = ZLayer(
    ZIO.serviceWithZIO[ULogs](_.forService[FooBarService])
      .map(new FooBarService(_))
  )
}
```

Then look at the main app:

```scala
object Main extends ZIOAppDefault {
  def run = {
    for {
      fooBar <- ZIO.service[FooBarService]
      _ <- fooBar.doLogs
      //...
    } yield ()
  }.provide(
    FooBarService.layer,
    ZLogs.layerPlainWithContext,
    TofuDefaultContext.layerZioContextOn
  ) @@ ZIOAspect.annotated("foo", "bar") @@ httpCode(204)
}
```

The output of this program will be:

```json
{
  "level": "INFO",
  "logger_name": "tofu.logging.zlogs.FooBarService",
  "message": "Got current date 2022-09-20",
  "today": "2022-09-20",
  "httpCode": 204,
  "zAnnotations": {
    "foo": "bar"
  }
}
```

If `TofuDefaultContext.layerZioContextOff` was used instead of `layerZioContextOn`, `zAnnotations` wouldn't be logged.
