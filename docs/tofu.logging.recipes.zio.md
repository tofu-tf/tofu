---
id: tofu.logging.recipes.zio
title: ZIO Logging
---

## ZIO logging
To use logging functional adapted for ZIO users, first add the following dependency:

```sbt
libraryDependencies += "tf.tofu" %% "tofu-zio-logging" % "<latest version in the badge in README>"
```
Then import the package:
```scala
import tofu.logging.zlogs._
```
This contains some useful stuff:

* `tofu.logging.Logging` type aliases\
These services do logs.
  - `ULogging` — is a type alias for `Logging[UIO]`. Logging methods have no environment like 
  `def info(message: String, values: LoggedValue*): UIO[Unit]`
  - `ZLogging[R]` — is a type alias for `Logging[URIO[R, *]]`. Logging methods require a ZIO environment `R`:
  `def info(message: String, values: LoggedValue*): URIO[R, Unit]`
  

* `ZLogging.Make` type\
  Use this instead of `tofu.logging.Loging.Make`. `Make` is a factory creating `Logging` instances with no side effects.
  - `ZLogging.Make` — is a type alias for `Logs[Id, UIO]`, produces plain instances of `ULogging`.
  - `ZLogging.ZMake[R]` — is a type alias for `Logs[Id, URIO[R, *]]`, produces contextual `ZLogging[R]`.

  Read more about logging factory in [core concepts](./tofu.logging.main.entities.md).


* `ZLogging.Make` object\
Provides several methods for creating ZIO layers with `Make` instances.
  - `layerPlain` creates layer contains simple implementation of `ZLogging.Make`
  - `layerContextual[R: Loggable]` makes a fabric `ZMake[R]` of contextual `ZLogging[R]` retrieving a context from 
  the ZIO environment of the logging methods
  - `layerPlainWithContext[C: Loggable, ContextService](f: ContextService => UIO[C])` creates layers with an implementation 
  of `ZLogging.Make` encapsulated context inside. Every logging methods will call function `f` on the `ContextService` 
  to get a context which will be added to the logs. The `ContextService` is supposed to be provided at the app creation point 
  via ZLayer environment.

### Example
Let's write a simple service, which logs a current date. 
```scala
import tofu.logging.zlogs._
import tofu.syntax.logging._
import zio._
import zio.clock._

trait BarService {
  def foo: Task[Unit]
}

class BarServiceImpl(clock: Clock.Service, logs: ZLogging.Make)
  extends BarService {

  private implicit val log: ULogging = logs.forService[BarServiceImpl]

  override def foo: Task[Unit] =
    for {
      _ <- log.info("Start program")
      dt <- clock.localDateTime
      date = dt.toLocalDate
      _ <- debug"Got current date $date"
    } yield ()
}

object BarService {
  val live: URLayer[Clock with TofuLogs, Has[BarService]] = (new BarServiceImpl(_, _)).toLayer
}
```

What can we learn from this code?
1. According to ZIO [Module Pattern 2.0](https://zio.dev/1.x/datatypes/contextual/index#module-pattern-20) 
class constructors are used to define service dependencies. At the end of the day the class constructor
is lifted to ZLayer: `(new BarServiceImpl(_, _)).toLayer`
2. `TofuLogs` is a type alias for `Has[ZLogging.Make]`, but ZIO encourages us to use explicitly the `Has` wrapper 
whenever we want to specify the dependency on a service.
3. Logging methods can be used:
   * explicitly like `log.info("Start getting datetime")`
   * via string interpolation provided by [tofu.syntax.logging](./tofu.logging.syntax.md). For this purpose 
   log service was defined as `implicit`.
4. To log object Tofu must know how to present it in the log message. This way is described by instances of `Loggable`
type class. We did provide no one because Tofu already has `java.time` `Loggable` instances.

Now look at the main app:
```scala
import tofu.logging.zlogs.ZLogging
import zio._
import zio.magic._

object Main extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    for {
      barService <- ZIO.service[BarService]
      _ <- barService.foo
    } yield ()
  }.injectCustom(
    BarService.live,
    ZLogging.Make.layerPlain
  ).exitCode
}
```
Thanks to [zio-magic](https://github.com/kitlangton/zio-magic/) we can just list all dependencies in 
the `injectCustom` method, not design layers manually. Output of the program will be:
```json lines
{"level":"INFO","message":"Start program"}
{"level":"DEBUG","message":"Got current date 2021-09-20"}
```
**Note:** for simplicity here and further extra fields (e.g. *timestamp*, *threadName*) were removed.

### Custom `Loggable`
Default `Loggable[LocalDate]` is based on `stringValue` instance, hence the date was logged as a plain string, not key-value. 
What if you want the date to be a separated field in json? Well, you can customize `Loggable`. Add the following line into the `BarServiceImpl`:
```scala
private implicit val customDateLoggable = Loggable[LocalDate].named(name="date")
```
Now the output looks like:
```json lines
{"level":"DEBUG","message":"Got current date 2021-09-20","date":"2021-09-20"}
```
`Loggable.apply[LocalDate]` summons an instance from the global scope, `.named` converts logged object into a single field named `name`.
There are several methods to create and modify `Loggable` instances, read more in [Loggable](./tofu.logging.loggable.md) section.

### Context logging
Let's consider how to log the context along with actual log message. If you have an instance of `Loggable` for your context, 
you can have it logged automagically. For example:
```scala
import derevo.derive
import tofu.logging.derivation.loggable

@derive(loggable)
final case class Context(requestId: Int)
```
Here we use [Tofu Derevo](https://github.com/tofu-tf/derevo) for automatic derivation [Loggable](./tofu.logging.loggable.md) instance.

One possible way to add a context to your logs is to use `layerPlainWithContext` which encapsulates dealing with the context inside
(otherwise you can use `layerContextual` retrieving the context from a ZIO environment `R`, but we won't cover it here).

The main idea of this approach is to store your context in ZIO [FiberRef](https://zio.dev/1.x/datatypes/fiber/fiberref). 
It provides all the power of State Monad. Unlike Java's `ThreadLocal`, `FiberRef` has copy-on-fork semantic: 
a child [Fiber](https://zio.dev/1.x/datatypes/fiber/fiber/) starts with `FiberRef` values of its parent.
When the child set a new value of FiberRef, the change is visible only to the child itself. This means if we set `requestId` value to `117`
(e.g. at the start of the request) and pass the `FiberRef` to a child fiber, it sees the value `117`.

Let's modify Main app.  We have to define a context service and how it provides the context. 
```scala
object Main extends zio.App {
  val contextLayer: ULayer[Has[FiberRef[Context]]] = FiberRef.make(Context(-10)).toLayer

  val logsLayer: URLayer[Has[FiberRef[Context]], TofuLogs] = ZLogging.Make.layerPlainWithContext(_.get)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    for {
      (barService, ref) <- ZIO.services[BarService, FiberRef[Context]]
      f1 <- barService.foo.fork
      _ <- ref.set(Context(117))
      f2 <- barService.foo.fork
      _ <- f1.join <&> f2.join
    } yield ()
  }.injectCustom(
    BarService.live,
    logsLayer,
    contextLayer
  ).exitCode
}
```
Run the program and look at the output:
```json lines
{"level":"INFO","message":"Start program","requestId":-10}
{"level":"DEBUG","message":"Got current date 2021-09-20","requestId":-10,"date":"2021-09-20"}
{"level":"INFO","message":"Start program","requestId":117}
{"level":"DEBUG","message":"Got current date 2021-09-20","requestId":117,"date":"2021-09-20"}
```
As you can see, the context has been added to log messages without any changes to the service calling the logging methods. 