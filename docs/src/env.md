---
id: env
title: Env
---

## Installation
`"ru.tinkoff" %% "tofu" % tofu-version`  
or as a standalone dependency:   
`"ru.tinkoff" %% "tofu-env" % tofu-version`  

## What is Env, once again?

Env is a monad, allowing composition of functions that are context(environment)-aware.  
For example, you may have several functions that depend on some common environment/runtime.  
Env provides a nice and convenient way to compose such functions, allowing access to this environment in a monadic way.

## Features  

### Access to environment
You can easily access passed environment at any time in a clean monadic way 
```scala
import tofu.env._
import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

type MyContext = String
val printCtx: Env[MyContext, Unit] = Env.context[MyContext].flatMap(ctx => Env.delay(println(ctx)))

printCtx.run("I am a context").runSyncUnsafe(1.second) // will print 'I am a context'"
```

### Local overriding  
It is possible to override context locally for specific functions that you may want to use with another context.
```scala
import monix.execution.Scheduler.Implicits.global
import scala.concurrent.duration._
import tofu.env._

val printContext: Env[MyContext, Unit] = Env.context[MyContext].flatMap(ctx => Env.delay(println(ctx)))

val env: Env[MyContext, Unit] =
    for {
      _ <- printContext
      _ <- printContext.local(ctx => s"$ctx, but new!")
    } yield ()

env.run("I am a context").runSyncUnsafe(1.second)
// will print to console:
//I am a context
//I am a context, but new!
```  

### Monix compatibility
Under the hood, Env is just a function `E => Task[A]`.   
Since it's primary based on Monix task, it mirrors most of its methods and functions, including parallel execution, error handling,
memoization, forking and working with resources.  
Env plays well with Cats and Cats-Effect, providing instances for most of typeclasses (see `tofu.env.EnvInstances`), 
except `Effect` and `ConcurrentEffect` (which allow starting computation at any place, so it contradicts Env, which requires context being passed).


## Complete example
Below is a complete example of how Env can be used to pass some environment to computations, use it through the code
```scala
import monix.eval.Task
import scala.concurrent.duration._

object EnvExamples extends scala.App {

  /** This is sample context for our computation  */
  case class MyContext(userId: String)

  // These aliases are declared once in your codebase and are used everywhere later
  type MyEnv[A] = Env[MyContext, A]
  object MyEnv extends EnvSpecializedFunctions[MyContext]

  // Pure value, embedded in Env
  val f1: MyEnv[String] = MyEnv.pure("Hello")

  /** Will log everything passed with context values in form of `[user1] myCoolMessage` */
  def log(msg: String): MyEnv[Unit] =
    for {
      context <- MyEnv.context
      _       <- MyEnv.delay(println(s"[${context.userId}] $msg"))
    } yield ()

  // Env is composed of smaller functions, using all-known methods of flatMap, map, for-comprehensions etc.
  val env: MyEnv[Unit] =
    for {
      _   <- log("Start")
      str <- f1
      _   <- log(s"Result: $str")
      _   <- log("End")
    } yield ()

  // running Env with context yields value of type `monix.eval.Task`
  val task: Task[Unit] = env.run(MyContext("user1"))

  // evaluates Task, running our computation, will yield
  // [user1] Start
  // [user1] Result: Hello
  // [user1] End
  // Do not use runSyncUnsafe except on the edges of your beautiful pure program
  task.runSyncUnsafe(1.second)
}

```