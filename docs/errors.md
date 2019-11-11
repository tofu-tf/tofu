---
id: errors
title: Error management
---

## Error Handling

#### The problem
One big problem of MTL style is error handling.

Minimal error-enabled suite in cats is called [`ApplicativeError`](https://typelevel.org/cats/api/cats/ApplicativeError.html) and its problem is that you'll have full Applicative instances along with error-related methods.
, still you're not allowed to have several `FunctorRaise` or `ApplicativeError` instances since it will give you errors like that
```scala mdoc
    import cats.implicits._
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
Simplest solution here to create type class that isn't subtype of Functor 


```scala
trait Raise[F[_], E]{
  def raise[A](err: E): F[A]
}
```
, see also 
cats-mtl 's [`FunctorRaise`](https://typelevel.org/cats-mtl/mtl-classes/functorraise.html)

#### Possible solution
now you can write just
```scala mdoc
import tofu._
import tofu.syntax.raise._
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

