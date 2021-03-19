---
id: config
title: Config
---

## Installation
`"ru.tinkoff" %% "tofu" % tofu-version`  
or as a standalone dependency:   
`"ru.tinkoff" %% "tofu-config" % tofu-version`  

## Features
`tofu-config` is a boilerplate-free way to load configuration files as Scala classes, 
which provides:
* parallel error accumulation, which won't give up parsing upon the first error;
* out-of-the box support for case classes and sealed hierarchies through [`Magnolia`](https://github.com/propensive/magnolia) derivation,
sugared with [`Derevo`](https://github.com/manatki/derevo) macro-annotations;
* [`Typesafe Config`](https://github.com/lightbend/config) interoperability;
* ease of integration with custom configuration sources and types.

## Example
A simple groceries config:
```hocon
{
  "dairy": {
    "storeName": "Milkman's dream",
    "toBuy": ["eggs", "milk"]
  },
  "beverages": {
    "storeName": "Teaworld",
    "toBuy": [{
      "teaSort": "Pu'erh",
    }, {
      "coffeeSort": "Liberica"
    }]
  }
}
```
might be parsed into the following structure:
```scala
import cats.syntax.show._
import com.typesafe.config.ConfigFactory
import derevo.derive
import tofu.config.typesafe._

sealed trait Item

sealed trait Dairy extends Item
case object eggs extends Dairy
case object milk extends Dairy

sealed trait Beverage extends Item
case class Tea(teaSort: String) extends Beverage
case class Coffee(coffeeSort: String) extends Beverage

case class ItemStore[I <: Item](
  storeName: String,
  toBuy: List[I]
)

@derive(Configurable) 
case class Groceries(
  dairy: ItemStore[Dairy],
  beverages: ItemStore[Beverage]
)

val cfg = ConfigFactory.parseResources("groceries.conf")
syncParseConfig[Groceries](cfg) match {
  case Left(errors)     => errors.foreach(err => println(err.show))
  case Right(groceries) => println(groceries)
}
```
If we make a couple of errors in the config file, misspelling "eggs" and introducing beer type into beverages:
```hocon
{
  "dairy": {
    "storeName": "Milkman's dream",
    "toBuy": ["egs", "milk"]
  },
  "beverages": {
    "storeName": "Teaworld",
    "toBuy": [{
      "teaSort": "Pu'erh",
    }, {
      "coffeeSort": "Liberica"
    }, {
      "beerSort": "Sour"
    }]
  }
}
```
we will encounter verbose parsing errors:
```
dairy.toBuy.[0] : bad string 'egs' : expected one of: eggs,milk
beverages.toBuy.[2] : no variant found
```

## Abstractions
`tofu-config` comes with a couple of neat abstractions over config parsing. 
### Configurable
The typeclass, which defines the property of a specific type to be parsed from a config is called 
`Configurable` (the definition is simplified):
```scala
trait Configurable[A] { 
  def apply[F[_]](cfg: ConfigItem[F]): F[A]
}
```
where `ConfigItem` is a set of possible types of config elements (analog of `com.typesafe.ConfigValue`).

### ParallelReader
The context bound which is essential for config parsing is `ParallelReader`. It is defined as a wrapper for `cats.Parallel` 
and inferred from it:
```scala
final case class ParallelReader[F[_]](paralleled: Parallel[ConfigT[F, *]])
```

For a config to be parsed into a value of type `A` in the context of `F`, the following instances should be provided:
```scala
def parseCfg[F[_]: Refs: MonadThrow: ParallelReader, 
                A: Configurable](cfg: Config): F[A]
```
`MonadThrow` is for raising parsing errors.  
`ParallelReader` is for parallel parsing.  
`Refs` is for storing errors list inside a `Ref`.

## Custom types
Sometimes one needs to define custom ways to parse values from a config.
For example, we might want to provide a convenient syntax for request limits:
```
{
  "endpointA": "10 per 1 second",
  "endpointB": "1000 per 1 hour"
}
```
It can be done via a custom `Configurable` instance:
```scala
import com.typesafe.config.ConfigFactory
import derevo.derive
import tofu.config.ConfigError.BadString
import tofu.config.typesafe._
import tofu.syntax.raise._

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try

case class Limit(count: Int, duration: FiniteDuration)

val limitRegexp = "(\\d+) per ([0-9a-z ]+)".r

def fromString(string: String): Option[Limit] = string match {
  case limitRegexp(count, duration) =>
    Try(Limit(count.toInt, 
              FiniteDuration(Duration(duration).toSeconds, "seconds"))).toOption
  case _ => None
}

implicit val rateConfigurable: Configurable[Limit] =
  Configurable.stringConfigurable.flatMake { F => str =>
    fromString(str).orRaise(BadString(str, "Invalid limit rate"))(F.config, F.monad)
}

@derive(Configurable)
case class Limits(endpointA: Limit, endpointB: Limit)

println(syncParseConfig[Limits](ConfigFactory.parseResources("limits.conf")))
```

## Custom config sources
In order to provide a way to parse config from a custom source, one needs to provide a 
mapping from that source's types into `tofu.config.ConfigItem`.
For an example, please refer to the [`typesafe integration`](https://github.com/TinkoffCreditSystems/tofu/blob/master/config/src/main/scala/tofu/config/typesafe.scala#L21).


