---
id: tofu.logging.home title: Tofu Logging
---

# Welcome to the Tofu Logging Home page

This is a set of tools for logging. Configurable, concise, handy.

## Installation

Add these dependencies into your project:

```sbt
libraryDependencies += "tf.tofu" %% "tofu-logging" % "<latest version in the badge in README>"
libraryDependencies += "tf.tofu" %% "tofu-logging-derivation" % "<latest version in the badge in README>"

```

For ZIO users the following is enough:

```sbt
libraryDependencies += "tf.tofu" %% "tofu-logging-zio" % "<latest version in the badge in README>"
```

## Quick demo

```scala
type CardNumber = String //could be a newtype

@derive(loggable)
case class Client(name: String, @hidden cardNumber: CardNumber, id: UUID)

def processPayment(client: Client, amount: Long): IO[Result] =
  for {
    _ <- info"Processing payment for $client"
    _ <- warn"Amount $amount is lower than zero!".whenA(amount < 0)
    result <- processData(client, amount, "USD")
                        .onError(errorCause"Got error on processing payment for $client"(_))
  } yield result
```

## What's next
- Discover [the key features](./key-features.md)
- Get to know [the core concepts](./main-entities.md)
- Learn how to use [the syntax](./syntax.md)
- Find a way to use `logging` suitable for you in the [recipes](recipes/recipes.md)
- Check out [the examples](https://github.com/tofu-tf/tofu/tree/better-doobie-example/examples)

## Old documentation
You can also visit obsolete documentation [here](../logging.md).