---
id: memo
title: Memo
---

## Installation
`"ru.tinkoff" %% "tofu" % tofu-version`  
or as a standalone dependency:   
`"ru.tinkoff" %% "tofu-memo" % tofu-version`

## Functionality
* A cache of a single value on access.
* A cache of a values mapping on access.
* TTL. Expired values are discarded on access.
* Forced invalidation based on time.

There are no
* Infinite caching (workaround is ttl `FiniteDuration(Long.MaxValue, TimeUnit.NANOSECONDS)` = 292 days).
* Background renewal of cached values.
* Forced invalidation of single key (mapping cache).

## Examples
### Single value cache
```scala
import cats._
import cats.effect.Clock
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import tofu.common.Console
import tofu.concurrent._
import tofu.memo._
import tofu.syntax.console._
import tofu.syntax.monadic._

def effect[F[_] : Console: Functor]: F[Int] = putStrLn("called").as(335)

def f[F[_] : Console : Clock: Monad : Refs] =
  for {
    kasha <- Cached[F](effect)(10.minutes, CacheControl.empty.pure[F]).ref
    v1 <- kasha
    v2 <- kasha
    v3 <- kasha
  } yield List(v1,v2,v3).sum

f[Task].runSyncUnsafe()
```
### Value mapping cache
```scala:reset
import cats._
import cats.effect.Clock
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import tofu.common.Console
import tofu.concurrent._
import tofu.memo._
import tofu.syntax.console._
import tofu.syntax.monadic._


def effect[F[_] : Console: Functor]: Int => F[String] =
  x => putStrLn("called").as(s"Number $x")

def f[F[_] : Console : Clock: Monad: Refs] =
  for {
    kasha <- CachedFunc[F](effect)(10.minutes, CacheControl.empty.pure[F]).refVals.ref
    v1 <- kasha(335)
    _  <- putStrLn(v1)
    v2 <- kasha(335)
    _  <- putStrLn(v2)
    v3 <- kasha(336)
    _  <- putStrLn(v3)
  } yield ()

f[Task].runSyncUnsafe()
```

### TTL
```scala
import cats._
import cats.effect.{Clock, Timer}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import tofu.common.Console
import tofu.concurrent._
import tofu.memo._
import tofu.syntax.console._
import tofu.syntax.monadic._
import tofu.syntax.timer._


def longEffect[F[_]: Console: Monad: Timer]: Int => F[String] =
  x =>
    Timer[F].sleep(1.second) >>
    putStrLn("called").as(s"Number $x")

def f[F[_]: Console: Refs: Clock: Timer: Monad](ttl : FiniteDuration) =
  for {
    kasha <- CachedFunc[F](longEffect)(ttl, CacheControl.empty.pure[F]).refVals.ref
    v1 <- kasha(335)
    _  <- putStrLn(v1)
    _  <- sleep(ttl - 2.seconds)
    v2 <- kasha(335)
    _  <- putStrLn(v2)
    _  <- sleep(3.seconds)
    v3 <- kasha(335)
    _  <- putStrLn(v3)
  } yield ()

f[Task](10.seconds).runSyncUnsafe()
```
There is a pitfall with TTL. Cached value keeps time of access to cache and not time when the effect completes. Just keep it in mind. Modification of the last example 
```scala
def longEffect[F[_]: Console: Monad: Timer]: Int => F[String] =
  x =>
    sleep(3.seconds) >>
    putStrLn("called").as(s"Number $x")
```
returns

```
called
Number 335
called
Number 335
Number 335
```
### Forced invalidation
To invalidate cache update CacheControl and set it to current time:
```scala:reset
import cats._
import cats.effect.{Clock, Timer}
import java.util.concurrent.TimeUnit
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import tofu.common.Console
import tofu.concurrent._
import tofu.memo._
import tofu.syntax.console._
import tofu.syntax.monadic._
import tofu.syntax.timer._


def effect[F[_]: Functor: Console: Timer]: Int => F[String] =
  x => putStrLn("called").as(s"Number $x")

def f[F[_]: Monad :Refs : Clock: Timer: Console](ttl : FiniteDuration) =
  for {
    ref   <- Refs[F].of(CacheControl.empty)
    kasha <- CachedFunc[F](effect[F])(ttl, ref.get).refVals.ref
    v1    <- kasha(335)
    _     <- putStrLn(v1)
    _     <- sleep(2.seconds)
    v2    <- kasha(335)
    _     <- putStrLn(v2)
    now   <- Clock[F].realTime(TimeUnit.MILLISECONDS)
    _     <- ref.update(_.copy(InvalidationTS(now)))
    _     <- sleep(3.seconds)
    v3    <- kasha(335)
    _     <- putStrLn(v3)
  } yield ()

f[Task](1000.seconds).runSyncUnsafe()
```
How it works. Cached value remembers time of read access during update (T_update). On any read access tofu selects a time to compare with T_update (update value when `T_selected > T_update`). Selection is based on a formula `max(T_now - TTL, T_CacheControl)`. If you set `T_CacheControl = T_now` then always T_CacheControl is more than T_update. After update of a value, T_update equals T_CacheControl.
