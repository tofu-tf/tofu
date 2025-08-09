---
title: Contextual Logging
sidebar:
  order: 105
---

# Working with context

For most of the apps it is crucial to add some kind of context for the logs. It makes working with logs simpler and
easier.

## Context abstractions

tofu.logging relies on set of contextual abstractions from tofu:

- `WithContext[F[_], C]` — describes the existence of `C` in `F`
- `WithLocal[F[_], C]` — same as latter but with ability to "temporarily" alter `C`
- `WithProvide[F[_], G[_], C]` — describes the fact that `G[A]` can be evaluated to `F[A]` with given `C` (like running
  Reader-monad)

More on that can be found in the context [documentation](/docs/contextual/withcontext).

## Structure and context

tofu.logging is a structured logging. It means that when you log a message it will be produced as JSON (or other
structure), not just plain text. This structure has fields in it and the context appears as a set of fields and values.
See Loggable [documentation](/docs/logging/core-concepts/#typeclass-loggablea) on how to configure that.

```scala
@derive(loggable)
case class RequestContext(traceId: Long, session: Session)

```

Here we describe the context of some request and also derive `Loggable` instance for it.

## Logs creation

Only the `Logs` factory carries information about context, while each of the logging instances doesn't have that
anywhere in API. Let's define the effect the logging would happen in:

```scala
import cats.effect.IO
import cats.data.ReaderT

type RequestIO[A] = ReaderT[IO, RequestContext, A]
```

Tofu has predefined instances of Contextual typeclasses for ReaderT (and also for ZIO in
the `tofu-zio` [module](https://github.com/tofu-tf/tofu/tree/better-doobie-example/modules/zio)).

The first step is to create appropriate `Logs`. Usually it is done at the point where you build your app:

```scala
implicit val logsMain: Logging.Make[IO] = Logging.Make.plain[IO]
implicit val logsContext: Logging.Make[RequestIO] = Logging.Make.contextual[RequestIO, RequestContext]
```

Most of the time your app works with two or more effects, one of those is main and has no notion of context at all. The
rest of the effects have some kind of context.

## Using created logs

Second step is to use  `Logs` to create `Logging` for your service. Here [service logging recipe](/docs/logging/recipes/service) is used,
but it actually doesn't matter, you can use whatever way you want:

```scala
class MyService[F[_] : MyService.Log] {
  def sayHi = info"Hi!"
}

```

This is a service we will use as example. Note, that **it has no `WithContext` bound** or something alike, just logging.
We don't want to add infrastructure stuff like context in here.

Now let's see the difference between running this with `IO` and `RequestIO`:

```scala

val ioservice = new MyService[IO] //implicitly derived logging from logsMain
ioservice.sayHi 
```

and the result is

```json
{
  "message": "Hi!",
  "level": "INFO"
}
```

And now with context:

```scala

val ioservice = new MyService[RequestIO] //implicitly derived logging from logsContext
ioservice.sayHi 
```

and the result is

```json
{
  "message": "Hi!",
  "level": "INFO",
  "traceId": 34234234,
  "session": {
    "...": "..."
  }
}
```

## Example
Check out the example [here](https://github.com/tofu-tf/tofu/tree/master/examples/ce2/src/main/scala-2/tofu/example/logging/service).

