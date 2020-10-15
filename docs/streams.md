# Streams

Functional streaming effects like `fs2` had become a very handy tool for modeling streaming pipelines. 
Nevertheless, such libraries have several drawbacks heavily affecting user code  and making their usage less convenient.
Tofu Streams offers a set of type-classes allowing to abstract functional streaming effects in a TF way.

## Type classes

### Emits

`Emits` lets you emit some collection of elements into a stream:

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

`Evals` allows you to evaluate some other effect inside a streaming effect.
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

`Merge` allows you to interleave two input streams non-deterministically:
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

`ParFlatten` allows you to run multiple streams inside an outer stream simultaneously merging
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

### Region

`Region` is responsible for managing resources withing a stream.
In the following example let's imagine we want `ping` to acquire connection and then send "PING" 
message continually until it is interrupted.

```scala
import cats.{Applicative, Defer, Monad, SemigroupK}
import tofu.streams._
import tofu.syntax.streams.region._
import tofu.syntax.streams.evals._
import tofu.syntax.streams.combineK._
import tofu.syntax.monadic._

trait Socket[F[_]] {
  def write(bytes: Array[Byte]): F[Unit]
  def close: F[Unit]
}

trait MakeSocket[F[_]] {
  def open[I[_]]: I[Socket[F]]
}

def ping[
  F[_]: Monad: SemigroupK: Defer: Region[*[_], G, E]: Evals[*[_], G],
  G[_],
  E
](socket: MakeSocket[G]): F[Unit] =
  region(socket.open[G])(_.close).flatMap { sock =>
    // `.repeat` is a special syntax from `tofu.syntax.streams.combineK` based on `SemigroupK` and `Defer`.
    eval(sock.write("PING".getBytes)).repeat
  }
```

### Pace

`Pace` lets you regulate the pace of a stream.
Looking at the `ping` implementation from the previous example you might have 
admitted that we don't need to send pings at the maximum possible rate. 
Let's throttle it down to 1 ping at 10 seconds:

```scala
import cats.{Applicative, Defer, Monad, SemigroupK}
import tofu.streams._
import tofu.syntax.streams.region._
import tofu.syntax.streams.evals._
import tofu.syntax.streams.combineK._
import tofu.syntax.streams.pace._
import tofu.syntax.monadic._

import scala.concurrent.duration._

trait Socket[F[_]] {
  def write(bytes: Array[Byte]): F[Unit]
  def close: F[Unit]
}

trait MakeSocket[F[_]] {
  def open[I[_]]: I[Socket[F]]
}

def ping[
  F[_]: Monad: SemigroupK: Defer: Region[*[_], G, E]: Evals[*[_], G]: Pace,
  G[_],
  E
](socket: MakeSocket[G]): F[Unit] =
  region(socket.open[G])(_.close).flatMap { sock =>
    eval(sock.write("PING".getBytes)).repeat.throttled(10.seconds)
  }
```

### Broadcast

`Broadcast` makes it possible to pipe outputs of a stream to several processors `F[A] => F[B]` 
simultaneously.

```scala
import tofu.streams.Broadcast
import tofu.syntax.streams.broadcast._

case class Order()

trait Stats[F[_]] {
  def process: F[Order] => F[Unit]
}

trait Shipping[F[_]] {
  def process: F[Order] => F[Unit]
}

def processOrders[F[_]: Broadcast](shipping: Shipping[F], stats: Stats[F]): F[Order] => F[Unit] =
  _.broadcast(shipping.process, stats.process)
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

## Abstracting from legacy API

In real applications we usually would have to deal with API exposing concrete stream datatype.
In order to abstract from it all we need is `type LiftStream[S[_], F[_]] = Lift[Stream[F, *], S]`.

```scala
import cats.tagless.FunctorK
import fs2.Stream
import derevo.derive
import tofu.higherKind.derived.representableK
import tofu.fs2.LiftStream

case class Event(name: String)

@derive(representableK)
trait Consumer[F[_]] {
  def eventStream: F[Event]
}

object Consumer {

  // smart-constructor for `Consumer` lifting `Consumer[Stream[G, *]]` into `Consumer[F]` 
  // parametrized with an abstract streaming effect `F`.
  def make[F[_]: LiftStream[*[_], G], G[_]]: Consumer[F] =
    FunctorK[Consumer].mapK(new Impl[G])(LiftStream[F, G].liftF)

  // concrete implementation of consumer parametrized with `Stream[F, *]`
  final class Impl[F[_]] extends Consumer[Stream[F, *]] {
    def eventStream: Stream[F, Event] = ??? // implementation based on a legacy API exposing `Stream` datatype.
  }
}
```

## Apps built on Tofu Streams

[binance-aggregator](https://github.com/oskin1/binance-aggregator)
