# Streams

Functional streaming effects like `fs2` had become a very handy tool for modeling streaming pipelines. 
Nevertheless, such libraries have several drawbacks heavily affecting user code  and making their usage less convenient.
Tofu Streams offers a set of type-classes allowing to abstract functional streaming effects in a TF way.

## Type classes

### Emits

`Emits` allows to emit some collection of elements into a stream:

```scala
import cats.instances.list._
import tofu.streams.Emits
import tofu.syntax.streams.emits._

def names[F[_]: Emits]: F[String] = {
  val xs = List("Sandy", "Sally", "Susan")
  emits(xs)
}
```

### Evals

`Evals` allows to evaluate some other effect inside a streaming effect.
In the example below `Stream[IO, *]` can be put in place of `F[_]` and `IO` in place of `G[_]`. 

```scala
import cats.Monad
import tofu.streams.Evals
import tofu.syntax.monadic._
import tofu.syntax.streams.evals._

trait Metrics[F[_]] {
  def send(key: String, value: Long): F[Unit]
}

trait Logging[F[_]] {
  def info(msg: String): F[Unit]
}

case class Bid(amount: Long, ticker: String)

def stats[F[_]: Monad: Evals[*[_], G], G[_]](metrics: Metrics[G], logs: Logging[G]): F[Bid] => F[Unit] =
  in => eval(logs.info("Sending stats")) >> in.evalMap(bid => metrics.send(s"bid.${bid.ticker}.amount", bid.amount))
```

### Chunks

`Chunks` exposes an API for working with internal structure of a streaming effect - chunks.
Supposing we want to process a stream of elements in batches for performance reasons:

```scala
import cats.Foldable
import cats.syntax.foldable._
import tofu.streams._
import tofu.syntax.streams.evals._
import tofu.syntax.streams.chunks._

case class Order(baseAsset: String, quoteAsset: String, amount: Long, price: Long, ownerId: Long)

trait OrderBook[F[_]] {
  def add(orders: List[Order]): F[Unit]
}

def batchProcess[
  F[_]: Chunks[*[_], C]: Evals[*[_], G],
  G[_],
  C[_]: Foldable
](orderBook: OrderBook[G]): F[Order] => F[Unit] =
  _.chunks.evalMap(orders => orderBook.add(orders.toList))
```

### Temporal

`Temporal` is capable of taking temporal slices of a stream.
Consider a modified version of `batchProcess` from the previous example for `Chunks` 
working with chunks either accumulated withing 5 seconds or reached the size of 1000 elements:

```scala
import cats.Foldable
import cats.syntax.foldable._
import tofu.streams._
import tofu.syntax.streams.evals._
import tofu.syntax.streams.temporal._

import scala.concurrent.duration._

case class Order(baseAsset: String, quoteAsset: String, amount: Long, price: Long, ownerId: Long)

trait OrderBook[F[_]] {
  def add(orders: List[Order]): F[Unit]
}

def batchProcess[
  F[_]: Temporal[*[_], C]: Evals[*[_], G],
  G[_],
  C[_]: Foldable
](orderBook: OrderBook[G]): F[Order] => F[Unit] =
  _.groupWithin(1000, 5.seconds).evalMap(orders => orderBook.add(orders.toList))
```

### Merge

`Merge` allows to interleave two input streams non-deterministically:
In the example below we want `printFakeOfTrue` to continually print either fake or true facts pulled from
two different sources respectively:

```scala
import tofu.common.Console
import tofu.streams._
import tofu.syntax.streams.evals._
import tofu.syntax.streams.merge._
import tofu.syntax.console._

case class Fact(value: String)

trait Facts[F[_]] {
  def stream: F[Fact]
}

def printFakeOfTrue[F[_]: Merge: Evals[*[_], G], G[_]: Console](
  fakes: Facts[F],
  truth: Facts[F]
): F[Unit] =
   (fakes.stream merge truth.stream).evalMap(fact => putStrLn(fact.value))
```

### ParFlatten

`ParFlatten` allows to run multiple streams inside an outer stream simultaneously merging
their outputs into a single stream non-deterministically.
In the example below we want to run multiple processes concurrently:

```scala
import cats.instances.list._
import tofu.common.Console
import tofu.streams._
import tofu.syntax.streams.emits._
import tofu.syntax.streams.parFlatten._
import tofu.syntax.console._

case class SensorData()

trait Sensor[F[_]] {
  def run: F[SensorData]
}

def readDataFromAllSensors[F[_]: ParFlatten: Emits](sensors: List[Sensor[F]]): F[SensorData] =
  emits(sensors.map(_.run)).parFlattenUnbounded
```

### Compile

`Compile` provides methods for materialization of a stream in several ways:

First use case is to simply project a process described in terms of a streaming 
effect into a regular effect like `IO` ignoring the result:

```scala
import tofu.streams._
import tofu.syntax.streams.compile._

trait Cats[F[_]] {
  def print: F[Unit] // prints various cats into a console continually
}

def runCatsPrinter[F[_]: Compile[*[_], G], G[_]](cats: Cats[F]): G[Unit] =
   cats.print.drain
```

In some other cases we might want to materialize a stream into a collection of elements:

```scala
import tofu.streams._
import tofu.syntax.streams.compile._

case class Cat(name: String)

trait Cats[F[_]] {
  def emit: F[Cat] // emit various cats continually
}

def catsList[F[_]: Compile[*[_], G], G[_]](n: Int)(cats: Cats[F]): G[LazyList[Cat]] =
   cats.emit.to[LazyList]
```
