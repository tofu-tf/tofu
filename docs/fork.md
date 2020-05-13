---
id: fork
title: Forking and Racing
---



## Fire
A simple typeclass, which main responsibility is to start a process in the background and forget about the result value.
```scala
import tofu.env._
import tofu.syntax.fire._

// some long-running computation
val longComputation: Env[Unit, Unit] = Env.delay(())

// this is now running in the background, all errors will be silently discarded
val started: Env[Unit, Unit] = longComputation.fireAndForget
``` 

## Race
Allows to run two computations concurrently, returning the result of either completed first (or failed first).
```scala
import cats.Functor
import cats.syntax.functor._
import tofu.Race
import tofu.syntax.race._

// will result in two computations started in parallel, 
// then the first one to return will be handled
// the other will be canceled 
def raceAndPickFirst[F[_]: Race: Functor, A, B](f1: F[A], f2: F[B]): F[String] = {
  val aOrB: F[Either[A, B]] = f1.race(f2)
  
  aOrB.map {
    case Left(a)  => s"Return A: $a"
    case Right(b) => s"Return B: $b"
  }
}
``` 

## Start
Allows starting computation, resulting in `Fiber` which can then be joined on or canceled.
```scala
import cats.effect.Fiber
import cats.FlatMap
import cats.syntax.flatMap._
import tofu.Start
import tofu.syntax.start._

// will result in computation being started 
// in background and then immediately canceled
def startAndCancel[F[_]: Start: FlatMap, A](f: F[A]): F[Unit] = {
  val fiber: F[Fiber[F, A]] = f.start
  fiber.flatMap(_.cancel)
}
```

TBD.