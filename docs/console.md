---
id: console
title: Using Console
---

### What if you need to keep things simple?

Let's say you're writing something simple and you need some of that good ol' console IO. Of course you can use plain `println` but we are nice and pure here. It is way better to use `tofu.Console` in such case.

#### Console

The `Console[F]` does three things:

* lets you read from standard input in `F`
* lets you write to standard output in `F`
* lets you write to error output in `F`

Let's demonstrate it with simple example.
Suppose we are writing a subset of unix `cat` program that can echo its input to output.

```scala mdoc
import cats.effect.{ExitCode, IO, IOApp}
import tofu.common.Console
import cats.FlatMap
import tofu.syntax.monadic._ //for flatMap


object catStraight extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = 
       catProgramStep[IO].foreverM

  def catProgramStep[F[_] : FlatMap : Console]: F[Unit] = for {
    input <- Console[F].readStrLn 
    _ <- Console[F].putStrLn(input) // or putStr if you don't like newlines
  } yield ()
}
```

Where does the instance of `Console[IO]` comes from? 
The answer is that for any type `F` that has `Sync[F]` instance of console comes for free by using standard scala console IO.

#### Syntax

It's all cool but writing `Console[F]` isn't cool. There is 'tofu.syntax.console' for a fancy functions to work with it.
Let's make our cat program a little nicer by adding one import and removing duplicates.

```scala mdoc
import tofu.syntax.console._ //this one gets you all the goodies
object catWithSyntax extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    catProgramStep[IO].foreverM

  def catProgramStep[F[_]: FlatMap: Console]: F[Unit] = for {
    input <- readStrLn
    _ <- if (input != "dog")
          putStrLn(input)
        else
          putErrLn("Do not scare the cat!")
  } yield ()
}
```

So when ran it works like that:

```
cat
>cat
kitten
>kitten
dog
>Do not scare the cat! //written in red because of 'putErrLn'
```

##### Show

There are integrations with `cats.Show` typeclass.
Let's say we have some case class and a custom `Show` instance for it:

```scala mdoc
import cats.Show

case class Person(name: String)
implicit val personShow: Show[Person] = Show.show[Person](p => s"this person has name ${p.name}")
```

You can use two methods to put a person to console

```scala mdoc
val cat: Person = Person("Cat")

putToStringLn[IO](cat).unsafeRunSync() //uses .toString 
putShowLn[IO, Person](cat).unsafeRunSync() //uses Show from scope
```

##### Puts

Also, it is possible to print a interpolated string in a nice way using `puts"..."`:
```scala mdoc
def putCat[F[_]: Console]: F[Unit] = puts"$cat, not a Person"
putCat[IO].unsafeRunSync()
```
As you can see it uses `Show` inside so you'd want to have instances for values inside `puts`-string.
The error message when you do not have `Show` instance in scope look like that:
```sbtshell
[error] ...: type mismatch
[error]  found   : tofu.cat.Person
[error]  required: cats.Show.Shown
[error]   def putCat[F[_]: Console] = puts"$cat that is not a Person"
[error]                                     ^
```


