---
title: Key Features
sidebar:
  order: 3
---

### Supported for every popular style

You can use `tofu.logging` in any style you want:

- Tagless Final style;
- Simple `cats.effect.IO` or `(monix|zio).Task` and `ReaderT`;
- `ZIO` and ZLayers — with the help of dedicated module `tofu-zio-logging` and `ZLogs`;
- Even with `Future`s (although it's kinda in the past).

### Concise syntax

With the implicit instance of `Logging` one can log messages with ease:

```scala
import tofu.logging._
import tofu.syntax.logging._

val error = new Throwable("Oh no.")

def log[F[_] : Logging : Monad] = for {
  _ <- info"Hi! I'm logging"
  _ <- warn"Hello again!"
  _ <- error"It's me, error!"
  _ <- errorCause"So sad, I've got an error"(error)
} yield ()
```

### Structured and controllable

You can easily put any necessary values into the structure of the log message with `Loggable`; also you can fully
control what is logged and what is not:

```scala
import derevo._
import tofu.logging._

@derive(Loggable)
case class Payment(id: String, @hidden cardNumber: String, amount: Long)

def log[F[_] : Logging : Monad](payment: Payment) = info"Got payment $payment"
```

Given that there's a defined `Loggable` instance for this context and provided logger is context aware (e.g. created
with `contextual` method), every logged message will contain information about context, for example:

```json
{
  "message": "Got payment payment",
  "level": "INFO",
  "payment": {
    "id": "131234234",
    "amount": 432
  }
}
```

Note that card number is not present at all as it was `@hidden`.

### Context support

Let's say your effect type has some context — it could be trace id or some domain info:

```scala
import cats._
import cats.effect._

case class TraceId(id: Long)

type TracedIO[A] = ReaderT[TraceId, IO, A]
```

tofu.logging can extract it and automatically add into the structure of every log message:

```scala
class MyService[F[_] : Logging] {
  def sayHello: F[Unit] = info"Hello!"
}

val ioservice = new MyService[IO]
val tracedService = new MyService[TracedIO]
```

(_Note the absence of anything related to context in MyService, logging doest it all itself._)

Now if we run `tracedService.sayHello` the log message structure will contain the trace id:

```json
{
  "message": "Hello!",
  "level": "INFO",
  "trace.id": 64534
}
```

and if we run `ioService.sayHello` the message will be clean:

```json
{
  "message": "Hello!",
  "level": "INFO"
}
```

More on that can be found on the dedicated [recipe page](/docs/logging/recipes/context).

## What's next

- You can read about core concepts [here](/docs/logging/core-concepts)
- You can see the recipes and discover what you need to use `logging` [here](/docs/logging/recipes/list)