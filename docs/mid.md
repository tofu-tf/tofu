---
id: mid
title: Mid
---

## Installation
`"ru.tinkoff" %% "tofu" % tofu-version`  
or as a standalone dependency:   
`"ru.tinkoff" %% "tofu-core-higher-kind" % tofu-version` 

## Assumption

Consider some trait
```scala
trait MyBusinessModule[F[_]] {
  def doBusinessThing(entity: Entity, info: Info): F[Value]
  def undoBusinessThing(entity: Entity): F[Respect]
}
```
Often `F` presented like some `IO`, reader, or any transformer

But signature doesn't oblige to be strict. Moreover, there is no necessity to use a functor

Let's start with an example
```scala
type Pre[F[_], A] = F[Unit]
``` 
Despite `Pre` has type-parameter `A`, it doesn't put any information to the result

Apply `MyBusinessModule[F[_]]` to `Pre[F[_], *]`
```scala
trait MyBusinessModule[Pre[F, *]] {
  def doBusinessThing(entity: Entity, info: Info): F[Unit]
  def undoBusinessThing(entity: Entity): F[Unit]
}
```
Only the effect is produced without any result. It could be logging, input validation, or something like that

Now consider the following type
```scala
type Post[F[_], A] = A => F[Unit]
```
This is a contravariant type. The module takes the form
```scala
trait MyBusinessModule[Post[F, *]] {
  def doBusinessThing(entity: Entity, info: Info): Value => F[Unit]
  def undoBusinessThing(entity: Entity): Respect => F[Unit]
}
```
Such an implementation of a module can express logging or validation of a computation result

Completes the next type
```scala
type Mid[F[_], A] = F[A] => F[A]
``` 
With `Monad[F]` both `Pre` and `Post` can be turned into `Mid`

Applying this to the module
```scala
trait MyBusinessModule[Mid[F, *]] {
  def doBusinessThing(entity: Entity, info: Info): F[Value] => F[Value]
  def undoBusinessThing(entity: Entity): F[Respect] => F[Respect]
}
```
`Mid` provides capabilities of both `Pre` and `Post`, but also allows to run `F` multiple times or not to run it at all.

Such middleware can be caching, retrying, or another logic, which is not implemented in infrastructure but requires additional reflection

## Usage

It turns out that [ApplyK](https://typelevel.org/cats-tagless/api/cats/tagless/ApplyK.html) is enough. Via
```scala
def map2K[F[_], G[_], H[_]](af: A[F], ag: A[G])(f: Tuple2K[F, G, *]~> H]): A[H]
```
It makes it possible to compose the result of the main computation and the result of a plug-in computation. Hence, we can also compose the main module implementation and pluggable one

Calling `map2K` with `F = F, G = Mid[F, *], H = F`, then substituting `MyBusinessModule[F]` and plugin `MyBusinessModule[Mid]` 
as `af` and `ag`, only remains to implement `Tuple2K[F, G, *]~> H]` i.e. the polymorphic function `[A] (F[A], F[A] => F[A]) => F[A]` or `(fa, f) => f(fa)`

So plugin application is just the process of applying the function to the result of every method. The macro generating `ApplyK[MyBusinessModule]` will do the rest of all

## Example

Example `representableK` can be found in the [source](https://github.com/TinkoffCreditSystems/tofu/blob/master/doobie/src/test/scala/tofu/doobie/example/TofuDoobieExample.scala)

Example `applyK` for authorship of https://t.me/ppressives

```scala
import cats.{Applicative, FlatMap, Monad}
import cats.syntax.semigroup._
import derevo.derive
import derevo.tagless.applyK
import tofu.higherKind.Mid
import tofu.syntax.monadic._

trait Metrics[F[_]] {
  def timed[A](metricsKey: String)(f: F[A]): F[A]
}

trait Logger[F[_]] {
  def info(str: String): F[Unit]
}

@derive(applyK)
trait FooService[F[_]] {
  def foo(a: String): F[Int]
}

object FooService {
  def create[F[_] : Monad](metrics: Metrics[F], logger: Logger[F]): FooService[F] = {
    val mid = (new FooLogging(logger): FooService[Mid[F, *]]) |+| (new FooMetrics(metrics): FooService[Mid[F, *]])
    mid attach new FooImpl[F]
  }

  private final class FooImpl[F[_]: Applicative] extends FooService[F] {
    def foo(a: String): F[Int] = a.length.pure[F]
  }

  private final class FooLogging[F[_]: FlatMap](logger: Logger[F]) extends FooService[Mid[F, *]] {
    def foo(a: String): Mid[F, Int] =
      d => logger.info(s"Calling foo with a=$a") *> d.flatTap(res => logger.info(s"foo returned $res"))
  }

  private final class FooMetrics[F[_]](metrics: Metrics[F]) extends FooService[Mid[F, *]] {
    def foo(a: String): Mid[F, Int] = metrics.timed("timings.foo")(_)
  }
}
```
