---
id: corecatsmtlinterop
title: Cats MTL interop
---

## Core: Interop with cats MTL library

Obviously, this is a module inside `TOFU` for interop between some [Cats MTL](https://github.com/typelevel/cats-mtl) typeclasses and `tofu-core`

For example, you have external code using `Cats MTL` like this:

```scala
  import cats.mtl.Ask
  import cats.Functor
  import cats.syntax.functor._

  def externalProgram[F[_]: Functor](m: Int, f: String => String)(implicit A: Ask[F, String]): F[String] = {
    for {
      ctx <- A.ask
      res = f(ctx) 
    } yield res * m
  }
```     

and your code using `TOFU` where you have different context typeclasses:
```scala
  import cats.effect.Sync
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import tofu.WithContext
                                                
  import scala.util.Random

  def yourProgram[F[_]: Sync: WithContext[*[_], String]]: F[String] = {
    for {    
      int <- Sync[F].delay(scala.util.Random.nextInt())
      result <- externalProgram[F](int, identity)
    } yield result
  }
```

so you have a problem - how we can easy use `extermalProblem`?
Usually programmers write your own implicits conversions, but `TOFU` could provide this instances for you!

## Install
For installation interop import dependency into your project: 
`"ru.tinkoff" %% "tofu-core-cats-mtl" % tofu-version` 
import it into your code files: 
```scala 
import tofu.core.interop.catsmtl.implicits._
```

### Conversions

In this module we provide some conversions to `Cats MTL` typeclasses from `TOFU` typeclasses:
 * [F[_], C] (Applicative[F], WithContext[F, C]) => Ask[F, C]
 * [F[_], C] (Applicative[F], WithLocal[F, C]) => Local[F, C]
 * [F[_], E] (Functor[F], Raise[F, E]) => Raise[F, E]
 * [F[_], E] (Applicative[F], Errors[F, E]) => Handle[F, E]

Also conversions to `TOFU` typeclasses from `Cats MTL` typeclasses:
 * [F[_], C] (Ask[F, C]) => WithContext[F, C]
 * [F[_], C] (Local[F, C]) => WithLocal[F, C]
 * [F[_], E] (Raise[F, E]) => Raise[F, E]
 * [F[_], E] (Functor[F, E], Handle[F, E]) => Errors[F, E]