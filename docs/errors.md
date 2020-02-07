---
id: errors
title: Error management
---

## Producing errors

#### The problem
One big problem of MTL style is error handling.

Minimal error-enabled suite in cats is called [`ApplicativeError`](https://typelevel.org/cats/api/cats/ApplicativeError.html) and its problem is that you'll have full Applicative instances along with error-related methods.
, still you're not allowed to have several `FunctorRaise` or `ApplicativeError` instances since it will give you errors like that
```scala mdoc
    import cats._
    case class ArithmeticError() extends Throwable
    case class ParseError() extends Throwable

    def divideBad[F[_]](x: String, y: String)(implicit 
        F1: MonadError[F, ArithmeticError],
        F2: MonadError[F, ParseError]): F[String] = 
        // can not use Monad / Applicative / Functor syntax here
        ???
```

So you are forced for your error type to have be one per type constructor.

#### Possible solution
Simplest solution here to create type class that isn't subtype of Functor 


```scala
trait Raise[F[_], E]{
  def raise[A](err: E): F[A]
}
```
, see also 
cats-mtl 's [`FunctorRaise`](https://typelevel.org/cats-mtl/mtl-classes/functorraise.html)


now you can write just
```scala mdoc
import tofu._
import tofu.syntax.raise._
import tofu.syntax.monadic._
import cats.effect.IO
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

#### Problem
`ApplicativeError` gives the following method for error handling 

```scala
  def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
```

Here if `f` does not fail `F[A]` should describe successfull computation. The types however does not catch this fact, since we have no type for `Unexeptional` partner. read more [here](https://typelevel.org/blog/2018/04/13/rethinking-monaderror.html)

Tofu  support several typeclasses targeting this problem, the simplest is 

```scala
trait RestoreTo[F[_], G[_]] {
  def restore[A](fa: F[A]): G[Option[A]]
}
```

which can be used to simple restoration from any failure condition

Next is 
```scala
trait HandleTo[F[_], G[_], E] extends RestoreTo[F, G] {
  def handleWith[A](fa: F[A])(f: E => G[A]): G[A]

  def handle[A](fa: F[A])(f: E => A)(implicit G: Applicative[G]): G[A] =
    handleWith(fa)(e => G.pure(f(e)))

  def attempt[A](fa: F[A])(implicit F: Functor[F], G: Applicative[G]): G[Either[E, A]] =
    handle(F.map(fa)(_.asRight[E]))(_.asLeft)
}
```

which can handle concrete error type like that

```scala mdoc
import cats.data.EitherT
import cats.instances.vector._
import cats._
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


HandleTo empowered with Raise is called `ErrorsTo`

```scala
trait ErrorsTo[F[_], G[_], E] extends Raise[F, E] with HandleTo[F, G, E]
```

There are also specialized versions of `RestoreTo`, `HandleTo` and `ErrorsTo` without `To`

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







