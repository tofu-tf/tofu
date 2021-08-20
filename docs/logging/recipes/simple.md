---
id: tofu.logging.recipes.simple
title: Tofu Logging recipes
---

## Simple logging
All you need to do to start using logging in its simplest (and kinda boilerplate-y) form is the following:
- add Logging.Make as parameter to your service — that will describe the capability to create `Logging`s;
- create logging instance with method `Logging.Make[F].forService`.

That's it!

```scala
class MyService[F[_]: Monad: Logging.Make](someDependency: DependencyService){

  private implicit val logging: Logging[F] = Logging.Make[F].forService[MyService[F]]
  
  def makeThis: F[Unit] = someDependency.foo(30) >> info"Something"

  def makeThat: F[Unit] = someDependency.foo(30).flatTap(result => warn"Some another thing $result")
}
```
At the app creation point you'll need to create the instance of Logging.Make by the method `Logging.Make.plain`:
```scala
def run: IO[ExitCode] = {
  implicit val makeLogging: Logging.Make[IO] = Logging.Make.plain[IO]
  
  val myService = new MyService[IO](???)
  
  ???//the rest of the app
}
```
And if you have context inside your `F` you can use method `Logging.Make.contextual[F, Ctx]`:
```scala
def run: IO[ExitCode] = {
  implicit val makeLogging: Logging.Make[ReaderT[IO, SomeContext, *]] = Logging.Make.contextual[ReaderT[IO, SomeContext, *], SomeContext]

  val myService = new MyService[ReaderT[IO, SomeContext, *]](???)

  ???//the rest of the app
}
```


