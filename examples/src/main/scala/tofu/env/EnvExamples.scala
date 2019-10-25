package tofu.env

import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

object EnvExamples extends App {

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

  // Monix's Task requires Scheduler to be started. Do not use global scheduler except for playground/testing purposes!
  implicit val scheduler: Scheduler = Scheduler.Implicits.global

  // evaluates Task, running our computation, will yield
  // [user1] Start
  // [user1] Result: Hello
  // [user1] End
  // Do not use runSyncUnsafe except on the edges of your beautiful pure program
  task.runSyncUnsafe(1.second)
}
