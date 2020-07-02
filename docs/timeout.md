---
id: timeout
title: Timeout
---

A simple typeclass, which allows you to timeout any process.

```scala mdoc
import cats.Monad
import cats.effect.Timer
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import tofu.{ Timeout, Raise }
import tofu.syntax.monadic._

import scala.concurrent.duration._


def computeValue[F[_]: Monad](implicit timer: Timer[F]): F[String] = 
    timer.sleep(3.seconds) >> "value".pure[F]

def program[F[_]: Monad: Timeout: Timer]: F[String] =
    Timeout[F].timeoutTo(computeValue, 1.seconds, "fallback".pure[F])

program[Task].runSyncUnsafe(Duration.Inf)
```

Timeout comes with handy syntax:

```scala mdoc
import tofu.syntax.timeout._

def program1[F[_]: Monad: Timeout: Timer]: F[Option[String]] =
    computeValue.timeout(1.seconds)

def program2[F[_]: Monad: Timeout: Timer]: F[String] =
    computeValue.timeoutOr(1.seconds, "fallback")

def program3[F[_]: Monad: Raise[*[_], Exception]: Timeout: Timer]: F[String] =
    computeValue.timeoutRaise(1.seconds, new Exception("boom"))

program1[Task].runSyncUnsafe(Duration.Inf)

program2[Task].runSyncUnsafe(Duration.Inf)

program3[Task].onErrorHandle(_ => "fallback").runSyncUnsafe(Duration.Inf)
```