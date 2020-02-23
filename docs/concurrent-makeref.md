---
id: concurrent-makeref
title: MakeRef
---

### Cats-Effect: Ref

An asynchronous, concurrent mutable reference.

Ref is a mutable reference which is non-blocking, accessed and modified concurrently, modified atomically and dealt with using only pure functions. The default implementation is nonblocking and lightweight, consisting essentially of a purely functional wrapper over an AtomicReference.

Provides safe concurrent access and modification of its content, but no functionality for synchronisation, which is instead handled by Deferred.
For this reason, a Ref is always initialised to a value.

```scala
abstract class Ref[F[_], A] {
  def get: F[A]
  def set(a: A): F[Unit]
  def modify[B](f: A => (A, B)): F[B]
  def update(f: A => A): F[Unit]
  // ... and more
}
```

Ref's companion object contains some methods which creates new Ref instance:

```scala
import cats.effect.Sync
import cats.effect.concurrent.Ref

object Ref {
    def of[F[_], A](a: A)(implicit F: Sync[F]): F[Ref[F, A]]  = ???
    def in[F[_], G[_], A](a: A)(implicit F: Sync[F], G: Sync[G]): F[Ref[G, A]] = ???
    //...
}
```

In fact, mutable content modifying is a side effect. Creating of mutable reference itself is a side effect too. Sometime we w
Ref helps to solve by offering some method to create instance which takes one or two type parameters as effect constructors.
Althought, this methods needs to given Sync instances for our effects. 

### Tofu: MakeRef

Tofu offers an easy and understandable initialyzer for Ref.  

```scala
import cats.effect.concurrent.Ref

trait MakeRef[I[_], F[_]] {
  def refOf[A](a: A): I[Ref[F, A]]
}
```

MakeRef has a companion object that offers easier initialization of Ref instances.
There is defined implicit syncInstance that helps in creating Ref on Sync based effects.

```scala
import tofu.concurrent.MakeRef
import cats.effect.Sync

object MakeRef {
  def apply[I[_], F[_]](implicit makeRef: MakeRef[I, F]) = ???
  implicit def syncInstance[I[_]: Sync, F[_]: Sync]: MakeRef[I, F] = ???
}
```

#### Ref creation
You can use object `MakeRef` that can produce values of type `I[Ref[F]]` (where `I` and `F` can be two different effects) and initialize it with , 
for example:  

```scala mdoc
import tofu.concurrent.MakeRef
import cats.effect.IO

def program: IO[(Int,Int)] =
    for {
      ref <- MakeRef[IO,IO].of[Int](42)
      c1  <- ref.get
      _   <- ref.modify(x => (x + 1, x))
      c2  <- ref.get
    } yield (c1,c2)

program.unsafeRunSync() // (42, 43)
```

You can simplify this by using Refs[F[]] type alias defined in `tofu.concurrent` package object. 

```scala mdoc
import tofu.concurrent.Refs
import cats.effect.IO

for {
    ref <- Refs[IO].of[Int](42)
    c1  <- ref.get
    _   <- ref.modify(x => (x + 1, x))
    c2  <- ref.get
} yield (c1,c2)
```

You can also omit the explicit indication of the value type.

```scala mdoc
import tofu.concurrent.Refs
import cats.effect.IO

for {
    ref <- Refs[IO].of(42)
    c1  <- ref.get
    _   <- ref.modify(x => (x + 1, x))
    c2  <- ref.get
} yield (c1,c2)
```