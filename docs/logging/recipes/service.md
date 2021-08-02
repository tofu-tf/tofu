---
id: tofu.logging.recipes.service
title: Tofu Logging recipes
---


## Service logging
You can also use tagged logging just for this service:

```scala
class MyService[F[_]: Monad: MyService.Log](someDependency: DependencyService){
  
  
  def makeThis: F[Unit] = someDependency.foo(30) >> info"Something"
  
  def makeThat: F[Unit] = someDependency.foo(30).flatTap(result => warn"Some another thing $result")
}

object MyService extends LoggingCompanion[MyService]
```

The line `object MyService extends LoggingCompanion[MyService]` mixes into the companion object type Log, which is just alias for `ServiceLogging[F, MyService]` (todo link to core concepts).

And in the wiring of the app:

```scala
import cats.effect.ExitCode
def run: IO[ExitCode] = {
  implicit val logs: Logging.Make[IO] = Logging.Make.plain[IO]
  
  val service = new MyService[IO](???)
}
```
The `MyService.Log` instance is created implicitly just for this service from implicit value of `Logging.Make`.
