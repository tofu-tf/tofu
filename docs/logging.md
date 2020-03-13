---
id: logging
title: Logging
---

### Logging

Almost every application nowadays does logging, whether it's logging of business events, history of some state of 
application or even debug record sequences. We log to files, distributed systems, standard outputs of OSes, and we 
got used to it so much it seems really simple to us. Many existing libraries provide many ways of logging from 
your application's code, either automatic or manual.

### What's wrong (and how to fix that)?

First, logging is, in fact, a side effect. Whether we log anything to standard output or a file - it's an interaction 
with the outside world ('outside' relative to our program). Thus, logging as a process can be represented as an effect and operations - as an algebra.  
Second, logging itself seems easy, but stuff around it - not so much. MDC, contexts, structured logging - all these 
things are very helpful on a day-to-day basis, allowing us to read and write our logs, making them clean, comprehensible 
and useful for many purposes. But there are few simple ways to provide all this functionality without bleeding and 
screaming from seeing over-engineered solutions.  
Tofu offers an easy and understandable abstraction over logging (it's even named like this!) but keeping in mind that 
logging is an effect that can be composed.  
It looks very similar to interfaces we all are used to (from SLF4J and other logging libraries and facades), introducing some new concepts:
```scala mdoc
import tofu.logging.LoggedValue

trait Logging[F[_]] {
    def trace(message: String, values: LoggedValue*): F[Unit]
    def debug(message: String, values: LoggedValue*): F[Unit]
    def info(message: String, values: LoggedValue*): F[Unit]
    def warn(message: String, values: LoggedValue*): F[Unit]
    def error(message: String, values: LoggedValue*): F[Unit]
}
```

Basically, that's the main user API you can interact with throughout your code. 
It resembles well-known solutions, keeping in mind that logging is an effect, though introducing the concept of `LoggedValue`.  


#### Logs representation (using Loggable)
To represent a value in logs we use a concept of `Loggable` (it's a typeclass). 
It describes how a value of some type can be logged, both as a string representation in log message 
and as a component of structured logging. Given an instance of `Loggable` for a type, a value of the type can be converted into the final internal representation called `LoggedValue` and thus logged in a way that you provided.  
There are multiple predefined ways to create an instance of `Loggable`, many of them can be found in `tofu.logging.Loggable` object:  
* `Loggable.empty` for no-op logging of value
* `Loggable.show` for using `cats.Show` instance as string representaion
* `Loggable.either` for logging either of `A` and `B`
* provided instances for all primitive types, as well as stdlib's collections and collections from Cats
* java.time.* instances

Of course, you can describe your `Loggable` instance yourself by extending existing traits that pre-implement some functionality:
* `DictLoggable` for multi-field objects
* `ToStringLoggable` for using `.toString` for logging
* `HideLoggable` to exclude value from logging

#### Loggable example
```scala mdoc
import tofu.logging._
import cats.syntax.semigroup._

case class User(name: String, surname: String)

implicit val userLoggable = new DictLoggable[User] {
  override def fields[I, V, R, S](a: User, i: I)(implicit r: LogRenderer[I, V, R, S]): R = {
    r.addString("name", a.name, i) |+| r.addString("surname", a.surname, i)
  }

  override def logShow(a: User): String = s"name = ${a.name}, surname = ${a.surname}"
}
```
Let's take a look at this example.  
First, we define a loggable for our `User` class as a `DictLoggable` which means
that we want to log it as a multi-field object with structure.  
Second, we define two methods that describe how `User` should be logged:
* `fields`, that represents `User` as a structure, containing two fields with their 
respective names and values
* `logShow`, that represents `User` as a string in a log message


#### Logging creation
Creating an instance of `Logging` in Tofu is an effect itself. You can use helper class `Logs` that can produce values of
type `I[Logging[F]]` (where `I` and `F` can be two different effects) and define a behaviour of your `Logging` instances. 
An instance of `Logs` is supposed to be shared by different parts of user code. though it's not a restriction.  
```scala mdoc
import tofu.logging._

// for simplification, you can use whatever F you like
type F[A] = cats.effect.IO[A]

class MyService

// defines simple logging behaviour without any notion of contexts
val syncLogs = Logs.sync[F, F]

// describing creation of logging instances and logging itself. When being run, this will log a message
val effect: F[Unit] = for {
    loggingBN <- syncLogs.byName("my-logger") // you can create instances by name
    loggingFS <- syncLogs.forService[MyService] // you can create instances by type tags
    _         <- loggingBN.info("This is an INFO message")
    _         <- loggingFS.warn("This is a WARN message")
} yield ()
```

There are multiple ways to create `Logs` instance, among them:
* `Logs.sync` that requires a `cats.effect.Sync` instance for delaying side-effects
* `Logs.withContext` that requires a `cats.effect.Sync` instance for delaying side-effects and a proof that `F` has a context to log
* `Logs.const` that wraps single `Logging` thus always returning the same instance
* `Logs.empty` that will produce a no-op `Logging` instances
* `Logs.combine` that combines two instances of `Logs`. 
`Logging` instance that is created this way will be a combination of two underlying instances, produced by both `Logs`. 
This means that both `Logging` implementations will be called in sequence    

#### Context logging
It is possible to log the context of your computations (MDC, remember?) if you have an instance of `tofu.Context` for your
effect type and a `Loggable` for your context. 
That's natural - describe how we can obtain a context from a computation `F` and a way to represent that context in log, 
and we will ensure it will be logged anytime you use `Logging` operations. 
You can log that context to string as well as use it as a part of structured logging.

Here, we defined our `Loggable` as a `DictLoggable` that has a notion of `fields`. 
Fields are a structure of your `Loggable` that is used for structured logging by your `LogRendered`.
Tofu comes with a `tofu.logging.ElkLayout` (present in `tofu-logging-layout` library), that will log the structure (fields) of your `Loggable`
as JSON fields.

#### Syntax extensions
It's much more convenient to use pre-defined syntax extensions for logging operations since they do all heavy lifting for you:
```scala mdoc:reset
import cats.{Monad, Show, Functor}
import cats.effect._
import cats.syntax.functor._
import tofu.logging._
import tofu.syntax.logging._

case class User(name: String, surname: String)
object User {
  implicit val userShow: Show[User]         = user => s"name=${user.name}, surname=${user.surname}"
  implicit val userInstance: Loggable[User] = Loggable.show
}

val user: User = User("name", "surname")

class MyService[F[_]: Logging: Monad] {
  def doLogging: F[Unit] =
    for {
      _ <- info"This is a syntax demo: $user"
    } yield ()
}

object MyService {
  def apply[I[_]: Functor, F[_]: Monad](implicit logs: Logs[I, F]): I[MyService[F]] = {
    logs.forService[MyService[F]].map(implicit l => new MyService[F])
  }
}

// An instance of `Logs` is supposed to be shared between mupltiple classes (or a single one for a whole application)
// Note that initializing effect of your application may differ from logging effect.
implicit val logs: Logs[IO, IO] = Logs.sync[IO, IO]

val io: IO[Unit] = for {
  service <- MyService[IO, IO]
  _       <- service.doLogging
} yield ()

io.unsafeRunSync()
// 13:56:26.918 [main] INFO tofu.logging.Test$MyService - This is a syntax demo: name=name, surname=surname
```

Using a syntax we achieve a few important goals.  
First, it will log a message containing a string representation of your value of type `User` as you defined it in `Loggable` instance.  
Second, it will create a `LoggedValue` from your `Loggable` description and pass it to underlying logger and your `LogRendered`, 
so your structured logging will work as expected. 