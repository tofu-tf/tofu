---
id: errors
title: Error management
---

## Producing errors

### Problem
One of the major issues of the MTL style is an error handling.

The weakest [`Cats`](https://typelevel.org/cats/) typeclass, which enables operations with errors, is an 
[`ApplicativeError`](https://typelevel.org/cats/api/cats/ApplicativeError.html).
 It brings a full `Applicative` instance apart from error-related methods and 
 this means, that we are not allowed to have a few `FunctorRaise` or `ApplicativeError` instances in the scope, since 
 their underlying `Functor`/`Applicative` instances will come into conflict:
```scala 
    import cats._
    case class ArithmeticError() extends Throwable
    case class ParseError() extends Throwable

    def divideBad[F[_]](x: String, y: String)(implicit 
        F1: ApplicativeError[F, ArithmeticError],
        F2: ApplicativeError[F, ParseError]): F[String] = 
        // using Functor / Applicative syntax here will cause an
        // "ambiguous implicit values" error
        ???
```

So we are forced to choose a single unified error type.

### Solution
The simplest solution here is to create a typeclass, that is not a subtype of Functor:


```scala
trait Raise[F[_], E]{
  def raise[A](err: E): F[A]
}
```
(see also 
cats-mtl 's [`FunctorRaise`](https://typelevel.org/cats-mtl/mtl-classes/functorraise.html)).


It would allow us to distinguish between different types of errors:
```scala 
import cats.effect.IO
import tofu._
import tofu.syntax.monadic._
import tofu.syntax.raise._

def divide[F[_]: Monad](x: String, y: String)(implicit 
    F1: Raise[F, ArithmeticError],
    F2: Raise[F, ParseError]
    ): F[String] = 
    ( x.toIntOption.orRaise(ParseError()),
      y.toIntOption.orRaise(ParseError())
       .verified(_ != 0)(ArithmeticError())
    ).mapN(_ / _).map(_.toString)

divide[IO]("10", "3").attempt.unsafeRunSync()

divide[IO]("10","0").attempt.unsafeRunSync()

divide[IO]("1", "0").attempt.unsafeRunSync()
        
```

## Recovering from errors

### Problem
`ApplicativeError` provides the following method for error handling:

```scala
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
```

Here, if `f` does not fail, `F[A]` should describe a successful computation. The types, however, do not convey this fact, 
since we have no type for `Unexeptional` partner. Read more [here](https://typelevel.org/blog/2018/04/13/rethinking-monaderror.html)

### Solution
`Tofu` is shipped with a few typeclasses targeting the problem. The simplest one is

```scala
trait RestoreTo[F[_], G[_]] {
  def restore[A](fa: F[A]): G[Option[A]]
}
```

which can be used to restore from any failure condition.

Another one is
```scala
trait HandleTo[F[_], G[_], E] extends RestoreTo[F, G] {
  def handleWith[A](fa: F[A])(f: E => G[A]): G[A]

  def handle[A](fa: F[A])(f: E => A)(implicit G: Applicative[G]): G[A] =
    handleWith(fa)(e => G.pure(f(e)))

  def attempt[A](fa: F[A])(implicit F: Functor[F], G: Applicative[G]): G[Either[E, A]] =
    handle(F.map(fa)(_.asRight[E]))(_.asLeft)
}
```

which can handle concrete error type:

```scala 
import cats._
import cats.data.EitherT
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.traverse._
import tofu._
import tofu.syntax.handle._
import tofu.syntax.monadic._
import tofu.syntax.raise._

def splitErrors[
  T[_]: Traverse: Alternative, 
  F[_]: Functor, G[_]: Applicative, E, A](ls: T[F[A]])(
    implicit errors: ErrorsTo[F, G, E]
): G[(T[E], T[A])] =
  ls.traverse(_.attemptTo[G, E]).map(_.partitionEither(identity))

def parseInt[F[_]: Applicative: Raise[*[_], String]](s: String): F[Int] =
  s.toIntOption.orRaise(s"could not parse $s")

type Calc[A] = EitherT[Eval, String, A]

splitErrors[Vector, Calc, Eval, String, Int](
  Vector("1", "hello", "2", "world", "3").map(parseInt[Calc])
).value
```


HandleTo, empowered with Raise, is called `ErrorsTo`:

```scala
trait ErrorsTo[F[_], G[_], E] extends Raise[F, E] with HandleTo[F, G, E]
```

There are also specialized versions of `RestoreTo`, `HandleTo` and `ErrorsTo` without `To`:

```scala
trait Restore[F[_]] extends RestoreTo[F, F] {
  def restoreWith[A](fa: F[A])(ra: => F[A]): F[A]
}

trait Handle[F[_], E] extends HandleTo[F, F, E] with Restore[F] {

  def tryHandleWith[A](fa: F[A])(f: E => Option[F[A]]): F[A]

  def tryHandle[A](fa: F[A])(f: E => Option[A])(implicit F: Applicative[F]): F[A] =
    tryHandleWith(fa)(e => f(e).map(F.pure))

  def handleWith[A](fa: F[A])(f: E => F[A]): F[A] =
    tryHandleWith(fa)(e => Some(f(e)))

  def recoverWith[A](fa: F[A])(pf: PartialFunction[E, F[A]]): F[A] =
    tryHandleWith(fa)(pf.lift)

  def recover[A](fa: F[A])(pf: PartialFunction[E, A])(implicit F: Applicative[F]): F[A] =
    tryHandle(fa)(pf.lift)

  def restoreWith[A](fa: F[A])(ra: => F[A]): F[A] = handleWith(fa)(_ => ra)
}


trait Errors[F[_], E] extends Raise[F, E] with Handle[F, E] with ErrorsTo[F, F, E]
```







