---
id: timeout
title: Timeout
---

Timeout typeclass allow to do useful effect operation - timeout.                                                          
Instance come with few handy syntax methods.                                                                              
The reason this typeclass's existing - 
abstract over default constraints for timeouting in Tagless Final approach provided by cats-effect: Concurrent and Timer.

```scala mdoc
import cats.Monad
import cats.effect.Timer

import tofu.{Timeout, Raise}
import tofu.syntax.monadic._

import scala.concurrent.duration._

//Demonstate program that computes after 3 seconds sleep
def computeValue[F[_]: Monad](implicit timer: Timer[F]): F[String] = 
  timer.sleep(3.seconds) >> "value".pure[F]

//Demonstate fallback with value in effect
def program[F[_]: Monad: Timeout: Timer]: F[String] =
  Timeout[F].timeoutTo(computeValue, 1.seconds, "fallback".pure[F])
```

Timeout comes with handy syntax:

```scala mdoc
import cats.Monad
import cats.effect.Timer

import tofu.{Timeout, Raise}
import tofu.syntax.timeout._

import scala.concurrent.duration._

//Demonstate program with timeout to optional value
def program1[F[_]: Monad: Timeout: Timer]: F[Option[String]] =
  computeValue.timeout(1.seconds)

//Demonstate program with timeout to pure fallback
def program2[F[_]: Monad: Timeout: Timer]: F[String] =
  computeValue.timeoutOr(1.seconds, "fallback")

//Demonstate program with timeout to raising an expection
def program3[F[_]: Monad: Raise[*[_], Exception]: Timeout: Timer]: F[String] =
  computeValue.timeoutRaise(1.seconds, new Exception("boom"))
```