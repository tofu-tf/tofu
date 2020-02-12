# Memo

## Dependency
It is in the core Tofu dependency - `"ru.tinkoff" %% "tofu" % Versions.tofu`.

## Functionality
* A cache of a single value on access.
* A cache of a values mapping on access.
* TTL. Expired values are discarded on access.
* Forced invalidation based on time.

Missed:
* Infinite caching (workaround is ttl `FiniteDuration(Long.MaxValue, TimeUnit.NANOSECONDS)` = 292 days).
* Background renewal of cached values.
* Forced invalidation of single key (mapping cache).

## Examples
### Single value cache
```scala
import tofu.memo._
import cats.effect.{Clock, Sync}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

def effect[F[_] : Sync]: F[Int] = Sync[F].delay(println("called")) >> 335.pure[F]

def f[F[_] : Sync : Clock] =
  for {
    kasha <- Cached[F](effect)(FiniteDuration(10L, TimeUnit.MINUTES), CacheControl.empty.pure[F]).ref
    v1 <- kasha
    v2 <- kasha
    v3 <- kasha
  } yield List(v1,v2,v3).sum

f[Task].runSyncUnsafe()

/*
called
res0: Int = 1005
*/
```
### Value mapping cache
```scala
import tofu.memo._
import cats.effect.{Clock, Sync}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

def effect[F[_] : Sync]: Int => F[String] =
  x => 
    Sync[F].delay(println("called")) >> 
    s"Number $x".pure[F]

def f[F[_] : Sync : Clock] =
  for {
    kasha <- CachedFunc[F](effect)(FiniteDuration(10L, TimeUnit.MINUTES), CacheControl.empty.pure[F]).refVals.ref
    v1 <- kasha(335)
    _ <- Sync[F].delay(println(v1))
    v2 <- kasha(335)
    _ <- Sync[F].delay(println(v2))
    v3 <- kasha(336)
    _ <- Sync[F].delay(println(v3))
  } yield ()

f[Task].runSyncUnsafe()

/*
called
Number 335
Number 335
called
Number 336
 */
```

### TTL
```scala
import tofu.memo._
import cats.effect.{Clock, Sync, Timer}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

def longEffect[F[_] : Sync : Timer]: Int => F[String] =
  x =>
    Timer[F].sleep(FiniteDuration(1L, TimeUnit.SECONDS)) >>
    Sync[F].delay(println("called")) >>
    s"Number $x".pure[F]

def f[F[_] : Sync : Clock : Timer](ttl : FiniteDuration) =
  for {
    kasha <- CachedFunc[F](longEffect)(ttl, CacheControl.empty.pure[F]).refVals.ref
    v1 <- kasha(335)
    _ <- Sync[F].delay(println(v1))
    _ <- Timer[F].sleep(ttl - FiniteDuration(2L, TimeUnit.SECONDS))
    v2 <- kasha(335)
    _ <- Sync[F].delay(println(v2))
    _ <- Timer[F].sleep(FiniteDuration(3L, TimeUnit.SECONDS))
    v3 <- kasha(335)
    _ <- Sync[F].delay(println(v3))
  } yield ()

f[Task](FiniteDuration(10L, TimeUnit.SECONDS)).runSyncUnsafe()

/*
called
Number 335
Number 335
called
Number 335
 */
```
There is a pitfall with TTL. Cached value keeps time of access to cache and not time when the effect completes. Just keep it in mind. Modification of the last example 
```scala
def longEffect[F[_] : Sync : Timer]: Int => F[String] =
  x =>
    Timer[F].sleep(FiniteDuration(3L, TimeUnit.SECONDS)) >>
    Sync[F].delay(println("called")) >>
    s"Number $x".pure[F]
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
```scala
import tofu.memo._
import cats.effect.{Clock, Sync, Timer}
import cats.effect.concurrent.Ref
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.TimeUnit

def effect[F[_] : Sync : Timer]: Int => F[String] =
  x =>
    Sync[F].delay(println("called")) >>
    s"Number $x".pure[F]

def f[F[_] : Sync : Clock : Timer](ttl : FiniteDuration) =
  for {
    ref <- Ref.of[F, CacheControl](CacheControl.empty)
    kasha <- CachedFunc[F](effect)(ttl, ref.get).refVals.ref
    v1 <- kasha(335)
    _ <- Sync[F].delay(println(v1))
    _ <- Timer[F].sleep(FiniteDuration(2L, TimeUnit.SECONDS))
    v2 <- kasha(335)
    _ <- Sync[F].delay(println(v2))
    now <- Clock[F].realTime(TimeUnit.MILLISECONDS)
    _ <- ref.update(_.copy(InvalidationTS(now)))
    _ <- Timer[F].sleep(FiniteDuration(3L, TimeUnit.SECONDS))
    v3 <- kasha(335)
    _ <- Sync[F].delay(println(v3))
  } yield ()

f[Task](FiniteDuration(1000L, TimeUnit.SECONDS)).runSyncUnsafe()

/*
called
Number 335
Number 335
called
Number 335
 */
```
How it works. Cached value remembers time of read access during update (T_update). On any read access tofu selects a time to compare with T_update (update value when `T_selected > T_update`). Selection is based on a formula `max(T_now - TTL, T_CacheControl)`. If you set `T_CacheControl = T_now` then always T_CacheControl is more than T_update. After update of a value, T_update equals T_CacheControl.
