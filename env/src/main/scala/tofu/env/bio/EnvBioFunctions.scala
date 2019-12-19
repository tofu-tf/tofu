package tofu.env.bio

import java.util.concurrent.TimeUnit.MILLISECONDS

import cats.Eval
import cats.data.ReaderT
import cats.effect._
import cats.syntax.either._
import monix.eval.{Coeval, Task}
import monix.execution.Scheduler

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

private[bio] trait EnvBioFunctions {
  def apply[R, E, A](f: R => Task[Either[E, A]]): EnvBio[R, E, A] = EnvBioCtx(f)
  def apply[R, A](f: R => Task[A]): EnvBio[R, Nothing, A]         = EnvUioCtx(f)
  def later[A](x: => A): EnvBio[Any, Nothing, A]                  = fromTask(Task.delay(x))
  def pure[A](x: A): EnvBio[Any, Nothing, A]                      = fromTask(Task.pure(x))

  def context[R]: EnvBio[R, Any, R]                       = apply(ctx => Task.now(ctx))
  def fromTask[A](task: Task[A]): EnvBio[Any, Nothing, A] = EnvUioCtx(_ => task)
}
