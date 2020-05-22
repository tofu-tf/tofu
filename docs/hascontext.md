---
id: hascontext
title: HasContext
---
## Installation
`"ru.tinkoff" %% "tofu" % tofu-version`  
or as a standalone dependency 
`"ru.tinkoff" %% "tofu-core" % tofu-version`  

## What if you don't need Env

Env is a powerful monad, but what if you're sure that you don't need it?
You can still use convenient Tofu concepts to work with your own Environment (`Context`).  

### Usage example and a short use case description  

The short story long, it is possible to use `ReaderT`:
 
```scala
import cats._
import cats.data.ReaderT
import cats.instances.option._
import tofu._
import tofu.optics._

// defining our own Env that stores some User
case class User(id: Int, name: String)
case class MyEnv(user: User)

// defining an extractor, extractor is a common lens that you can read about
// in a paragraph about lenses
implicit val extractor: Extract[MyEnv, User] = _.user
      
def program[F[_]: HasContext[*[_], MyEnv]](implicit u: MyEnv Extract User): F[String] = 
  Context[F].extract(u).ask(_.name)

// ~voilà
program[ReaderT[Option, MyEnv, *]].run(MyEnv(User(0, "Tofu"))) //> Some(Tofu): Option[String]

```

A bit more complicated example, that shows lenses usage only in the functions that require them:

```scala:reset
import cats._
import cats.data.ReaderT
import cats.instances.option._
import cats.syntax.apply._
import tofu._
import tofu.optics._

// defining our own Env that stores a User and some related Metadata
case class User(id: Int, name: String)
case class Metadata(height: Double, age: Int)
case class MyEnv(user: User, md: Metadata)

// defining extractors
implicit val userExtractor: Extract[MyEnv, User]   = _.user
implicit val mdExtractor: Extract[MyEnv, Metadata] = _.md

// it is possible to define a program that only has a context
def program[F[_]: Apply: HasContext[*[_], MyEnv]]: F[String] = 
  (name[F], age[F]).mapN { (name, age) => s"$name: $age" }

// but all the functions that were called inside a program
// have on demand and only necessary extractors
def name[F[_]: HasContext[*[_], MyEnv]](implicit u: MyEnv Extract User): F[String] = 
  Context[F].extract(u).ask(_.name)

def age[F[_]: HasContext[*[_], MyEnv]](implicit m: MyEnv Extract Metadata): F[Int] = 
  Context[F].extract(m).ask(_.age)

// ~voilà
program[ReaderT[Option, MyEnv, *]]
  .run(MyEnv(User(0, "Tofu"), Metadata(60, 18))) //> Some(Tofu: 18): Option[String]
```

It is also possible to do define some `Context` explicitly without having a need in `Env` or `ReaderT` monads:

```scala:reset
import cats._
import cats.instances.option._
import cats.syntax.apply._
import tofu._
import tofu.optics._

// defining our own Env that stores a User and some related Metadata
case class User(id: Int, name: String)
case class Metadata(height: Double, age: Int)
case class MyEnv(user: User, md: Metadata)

// defining extractors
implicit val userExtractor: Extract[MyEnv, User]   = _.user
implicit val mdExtractor: Extract[MyEnv, Metadata] = _.md

// what if we don't need or don't know what ReaderT is
// we can define a const Context than
implicit val ctx: HasContext[Option, MyEnv] =
  Context.const[Option, MyEnv](MyEnv(User(0, "Tofu"), Metadata(60, 18)))

// it is still possible to define a program that only has a context
def program[F[_]: Apply: HasContext[*[_], MyEnv]]: F[String] =
  (name[F], age[F]).mapN { (name, age) => s"$name: $age" }

// and all the functions that were called inside a program
// have on demand and only necessary extractors
def name[F[_]: HasContext[*[_], MyEnv]](implicit u: MyEnv Extract User): F[String] =
  Context[F].extract(u).ask(_.name)

def age[F[_]: HasContext[*[_], MyEnv]](implicit m: MyEnv Extract Metadata): F[Int] =
  Context[F].extract(m).ask(_.age)

// ~voilà
program[Option] //> Some(Tofu: 18): Option[String]
```
