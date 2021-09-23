---
id: tofu.logging.home
title: Home (Start here)
---

# Welcome to the Tofu Logging Home page

This is a set of tools for logging. Configurable, concise, handy.

## Installation

Add these dependencies into your project:

```sbt
libraryDependencies += "tf.tofu" %% "tofu-logging" % "<latest version in the badge in README>"
libraryDependencies += "tf.tofu" %% "tofu-logging-derivation" % "<latest version in the badge in README>"
```
`logging` requires an instance of the `Delay` typeclass, which can be created by hand, or can be imported from an according package:

Cats Effect 3 users should add the following dependency:
```sbt
libraryDependencies += "tf.tofu" %% "tofu-kernel-ce3-interop" % "<latest version in the badge in README>"
```

and Cats Effect 2 users should add:
```sbt
libraryDependencies += "tf.tofu" %% "tofu-kernel-ce2-interop" % "<latest version in the badge in README>"
```

For ZIO users the following is enough:

```sbt
libraryDependencies += "tf.tofu" %% "tofu-logging-zio" % "<latest version in the badge in README>"

```
See also [ZIO Logging](./tofu.logging.recipes.zio.md) section.

## Quick demo

```scala
import tofu.syntax.logging._
import tofu.logging.Logging
import derevo.derive
import tofu.logging.derivation.loggable

type CardNumber = String //could be a newtype

@derive(loggable)
case class Client(name: String, @hidden cardNumber: CardNumber, id: UUID)

def processPayment[F[_]: Monad: Logging](client: Client, amount: Long): F[Result] =
  for {
    _ <- info"Processing payment for $client"
    _ <- warn"Amount $amount is lower than zero!".whenA(amount < 0)
    result <- processData(client, amount, "USD")
                        .onError(errorCause"Got error on processing payment for $client"(_))
  } yield result
```

## What's next

- Discover [the key features](tofu.logging.key-features.md)
- Get to know [the core concepts](tofu.logging.main.entities.md)
- Learn how to use [the syntax](tofu.logging.syntax.md)
- Find a way to use `logging` suitable for you in the [recipes](tofu.logging.recipes.md)
- Check out [the examples](https://github.com/tofu-tf/tofu/tree/better-doobie-example/examples)

## Old documentation

You can also visit obsolete documentation [here](./logging.old.md).