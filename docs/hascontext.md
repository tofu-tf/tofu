---
id: hascontext
title: Learning HasContext
---

### What if you don't need Env

Env is a powerful monad, but what if you're sure that you don't need it?
You can still use convenient Tofu concepts to work with your own Environment (Context).  

#### Usage example and a short use case description  

The short story long, it is possible to use ReaderT:
 
```scala mdoc
import tofu.optics._
import tofu._

import cats.instances.option._
import cats._
import cats.data.ReaderT

// defining our own Env that stores some User
case class User(id: Int, name: String)
case class MyEnv(user: User)

// defining an extractor, extractor is a common lens that you can read about
// in a paragrapth about lenses
implicit val extractor: Extract[MyEnv, User] = _.user
      
def program[F[_]: HasContext[*[_], MyEnv]](implicit u: MyEnv Extract User): F[String] = 
  Context[F].extract(u).ask(_.name)

// ~voilà
program[ReaderT[Option, MyEnv, *]].run(MyEnv(User(0, "Tofu"))) //> Some(Tofu): Option[String]

```

