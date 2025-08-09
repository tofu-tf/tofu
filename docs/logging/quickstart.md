---
title: Quickstart
sidebar:
  order: 1
---

This is a set of tools for logging. Configurable, concise, handy.

## Installation

Add these dependencies into your project:

:::tip[Tofu Logging]{icon="download"}
```
"tf.tofu" %% "tofu-logging" % "@VERSION@"
"tf.tofu" %% "tofu-logging-derivation" % "@VERSION@"
```

:::


`logging` requires an instance of the `Delay` typeclass, which can be created by hand, or can be imported from an
according package:

Cats Effect 3 users should add the following dependency:

```
"tf.tofu" %% "tofu-core-ce3" % "@VERSION@"
```

and Cats Effect 2 users should add:

```
"tf.tofu" %% "tofu-core-ce2" % "@VERSION@"
```

For ZIO users the following is enough:

```
"tf.tofu" %% "tofu-zio2-logging" % "@VERSION@" // OR:
"tf.tofu" %% "tofu-zio-logging" % "@VERSION@"

```

See also [ZIO1](/tofu/docs/logging/recipes/zio1), [ZIO2](/tofu/docs/logging/recipes/zio2) Logging sections.

## Quick demo

```scala
import tofu.syntax.logging._
import tofu.logging.Logging
import derevo.derive
import tofu.logging.derivation.loggable

type CardNumber = String //could be a newtype

@derive(loggable)
case class Client(name: String, @hidden cardNumber: CardNumber, id: UUID)

def processPayment[F[_] : Monad : Logging](client: Client, amount: Long): F[Result] =
  for {
    _ <- info"Processing payment for $client"
    _ <- warn"Amount $amount is lower than zero!".whenA(amount < 0)
    result <- processData(client, amount, "USD")
      .onError(errorCause"Got error on processing payment for $client"(_))
  } yield result
```

## What's next

- Discover [the key features](/tofu/docs/logging/key-features)
- Get to know [the core concepts](/tofu/docs/logging/core-concepts)
- Learn how to use [the syntax](/tofu/docs/logging/syntax)
- Find a way to use `logging` suitable for you in the [recipes](/tofu/docs/logging/recipes/list)
- Check out [the examples](https://github.com/tofu-tf/tofu/tree/master/examples/ce2/src/main/scala-2/tofu/example/logging)

## Old documentation

You can also visit obsolete documentation [here](/tofu/docs/logging/old).
