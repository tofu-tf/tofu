package tofu.env

import cats.data.ReaderT
import cats.effect._
import monix.eval.Task
import monix.execution.annotations.UnsafeBecauseImpure
import monix.execution.{Cancelable, CancelableFuture, Scheduler}
import tofu.internal.IsTofu

import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** Env is a monad, allowing composition of functions that are context(environment)-aware.
  * For example, you may have several functions that depend on some common environment/runtime.
  * Env provides a way to compose such functions, allowing access to this environment in a monadic way.
  * It is possible to override context locally for concrete functions that you may want to use with another context.
  * Under the hood, Env is just a function `E => Task[A]`.
  * Since it's primary based on Monix task, it mirrors most of its methods and functions, including parallel execution, error handling,
  * memoization, forking and working with resources.
  * Env plays well with Cats and Cats-Effect, providing instances for most of typeclasses (see [[tofu.env.EnvInstances]]),
  * except Effect and ConcurrentEffect (which allow starting computation at any place, so it contradicts Env, which requires context being passed).
  */
sealed trait Env[E, +A] {
  def run(ctx: E): Task[A]
  def mapTask[R](f: Task[A] => Task[R]): Env[E, R]                                                                =
    Env(ctx => f(run(ctx)))
  def mapTask2[B, R](eb: Env[E, B])(f: (Task[A], Task[B]) => Task[R]): Env[E, R]                                  =
    Env(ctx => f(run(ctx), eb.run(ctx)))
  def mapTask3[B1, B2, R](e1: Env[E, B1], e2: Env[E, B2])(f: (Task[A], Task[B1], Task[B2]) => Task[R]): Env[E, R] =
    Env(ctx => f(run(ctx), e1.run(ctx), e2.run(ctx)))
  def mapTask4[B1, B2, B3, R](e1: Env[E, B1], e2: Env[E, B2], e3: Env[E, B3])(
      f: (Task[A], Task[B1], Task[B2], Task[B3]) => Task[R]
  ): Env[E, R]                                                                                                    =
    Env(ctx => f(run(ctx), e1.run(ctx), e2.run(ctx), e3.run(ctx)))
  def mapTask5[B1, B2, B3, B4, R](e1: Env[E, B1], e2: Env[E, B2], e3: Env[E, B3], e4: Env[E, B4])(
      f: (Task[A], Task[B1], Task[B2], Task[B3], Task[B4]) => Task[R]
  ): Env[E, R]                                                                                                    =
    Env(ctx => f(run(ctx), e1.run(ctx), e2.run(ctx), e3.run(ctx), e4.run(ctx)))
  def mapTask6[B1, B2, B3, B4, B5, R](e1: Env[E, B1], e2: Env[E, B2], e3: Env[E, B3], e4: Env[E, B4], e5: Env[E, B5])(
      f: (Task[A], Task[B1], Task[B2], Task[B3], Task[B4], Task[B5]) => Task[R]
  ): Env[E, R]                                                                                                    =
    Env(ctx => f(run(ctx), e1.run(ctx), e2.run(ctx), e3.run(ctx), e4.run(ctx), e5.run(ctx)))

  // execution
  def runNew(implicit provideEnv: ProvideEnv[E]): Task[A]                                          =
    provideEnv.provide.flatMap(run)
  def foreach(f: A => Unit)(implicit s: Scheduler, provide: ProvideEnv[E]): CancelableFuture[Unit] =
    provide.provide.flatMap(run).flatMap(a => Task.delay(f(a))).runToFuture

  def foreachL(f: A => Unit)(ctx: E): Task[Unit]                                  = run(ctx).foreachL(f)
  def foreachEnv(f: A => Unit): Env[E, Unit]                                      = mapTask(_.foreachL(f))
  def runOnComplete(f: Try[A] => Unit)(ctx: E)(implicit s: Scheduler): Cancelable =
    run(ctx).runAsync(e => f(e.toTry))

  /** convenient alias boosting inference */
  def local(f: E => E): Env[E, A]        = localP(f)
  def localT(f: E => Task[E]): Env[E, A] = localPT(f)

  /** polymorphic environment switch */
  def localP[E1](f: E1 => E): Env[E1, A]        = Env(ctx => run(f(ctx)))
  def localPT[E1](f: E1 => Task[E]): Env[E1, A] =
    Env(ctx => f(ctx).flatMap(run))
  def compose[B](g: Env[B, E]): Env[B, A]       = Env(b => g.run(b).flatMap(run))
  def andThen[B](g: Env[A @uv, B]): Env[E, B]   = g.compose(this)

  // basic transformations
  def map[B](f: A => B): Env[E, B]                                   = mapTask(_.map(f))
  def mapAsync[B](f: A => Task[B]): Env[E, B]                        = mapTask(_.flatMap(f))
  def flatMap[B](f: A => Env[E, B]): Env[E, B]                       =
    Env(ctx => run(ctx).flatMap(x => f(x).run(ctx)))
  def flatTap[B](f: A => Env[E, B]): Env[E, A]                       =
    flatMap(x => f(x).map(_ => x))
  def map2[B, C](eb: Env[E, B])(f: (A, B) => C): Env[E, C]           =
    mapTask2(eb)(Task.map2(_, _)(f))
  def map3[B, C, D](eb: Env[E, B], ec: Env[E, C])(f: (A, B, C) => D) =
    mapTask3(eb, ec)(Task.map3(_, _, _)(f))

  def parMap2[B, C](eb: Env[E, B])(f: (A, B) => C)                      =
    mapTask2(eb)(Task.parMap2(_, _)(f))
  def parMap3[B, C, D](eb: Env[E, B], ec: Env[E, C])(f: (A, B, C) => D) =
    mapTask3(eb, ec)(Task.parMap3(_, _, _)(f))

  def zip[B](t: Env[E, B]): Env[E, (A, B)] = map2(t)((a, b) => (a, b))
  def >>[B](t: => Env[E, B]): Env[E, B]    = flatMap(_ => t)
  def <<[B](t: => Env[E, B]): Env[E, A]    = flatTap(_ => t)
  def *>[B](t: Env[E, B]): Env[E, B]       = map2(t)((_, x) => x)
  def <*[B](t: Env[E, B]): Env[E, A]       = map2(t)((x, _) => x)
  def &>[B](t: Env[E, B]): Env[E, B]       = parMap2(t)((_, x) => x)
  def <&[B](t: Env[E, B]): Env[E, A]       = parMap2(t)((x, _) => x)

  def flatten[B](implicit _ev: A <:< Env[E, B]): Env[E, B] = flatMap(x => x)
  def flattenT[B](implicit _ev: A <:< Task[B]): Env[E, B]  = mapTask(_.flatten)
  def void: Env[E, Unit]                                   = mapTask(_.map(_ => ()))

  // error handling
  def onErrorHandle[B >: A](f: Throwable => B): Env[E, B]                                                  =
    mapTask(_.onErrorHandle(f))
  def onErrorHandleCtx[B >: A](f: (Throwable, E) => B): Env[E, B]                                          =
    Env(ctx => run(ctx).onErrorHandle(e => f(e, ctx)))
  def onErrorHandleCtxTask[B >: A](f: (Throwable, E) => Task[B]): Env[E, B]                                =
    Env(ctx => run(ctx).onErrorHandleWith(e => f(e, ctx)))
  def onErrorRecover[B >: A](f: PartialFunction[Throwable, B]): Env[E, B]                                  =
    mapTask(_.onErrorRecover(f))
  def onErrorRecoverWith[B >: A](f: PartialFunction[Throwable, Env[E, B]]): Env[E, B]                      =
    Env(ctx => run(ctx).onErrorRecoverWith(f.andThen(_.run(ctx))))
  def onErrorRecoverWithTask[B >: A](f: PartialFunction[Throwable, Task[B]]): Env[E, B]                    =
    mapTask(_.onErrorRecoverWith(f))
  def onErrorHandleWith[B >: A](f: Throwable => Env[E, B]): Env[E, B]                                      =
    Env(ctx => run(ctx).onErrorHandleWith(f.andThen(_.run(ctx))))
  def onErrorRestart(maxRetries: Long): Env[E, A]                                                          =
    mapTask(_.onErrorRestart(maxRetries))
  def onErrorRestartIf(f: Throwable => Boolean): Env[E, A]                                                 =
    mapTask(_.onErrorRestartIf(f))
  def onErrorRestartIfCtx(f: (Throwable, E) => Boolean): Env[E, A]                                         =
    Env(ctx => run(ctx).onErrorRestartIf(e => f(e, ctx)))
  def onErrorRestartLoop[S, B >: A](initial: S)(f: (Throwable, S, S => Env[E, B]) => Env[E, B]): Env[E, B] =
    onErrorHandleWith(err => f(err, initial, state => (this: Env[E, B]).onErrorRestartLoop(state)(f)))
  def onErrorRestartLoopTask[S, B >: A](initial: S)(f: (Throwable, S, S => Task[B]) => Task[B]): Env[E, B] =
    mapTask(_.onErrorRestartLoop[S, B](initial)(f))
  def onErrorFallbackTo[B >: A](that: Env[E, B]): Env[E, B]                                                =
    mapTask2(that)(_ onErrorFallbackTo _)
  def onErrorFallbackTask[B >: A](that: Task[B]): Env[E, B]                                                = mapTask(_.onErrorFallbackTo(that))

  def failed: Env[E, Throwable]                               = mapTask(_.failed)
  def materialize: Env[E, Try[A]]                             = mapTask(_.materialize)
  def dematerialize[B](implicit _ev: A <:< Try[B]): Env[E, B] =
    mapTask(_.dematerialize)
  def attempt: Env[E, Either[Throwable, A]]                   = map(Right.apply).onErrorHandle(Left.apply)

  def ensure(error: => Throwable)(predicate: A => Boolean): Env[E, A]             =
    mapTask(_.flatMap(a => if (predicate(a)) Task.pure(a) else Task.raiseError(error)))
  def ensureOr(error: A => Throwable)(predicate: A => Boolean): Env[E, A]         =
    mapTask(_.flatMap(a => if (predicate(a)) Task.pure(a) else Task.raiseError(error(a))))
  def ensureCtx(error: E => Throwable)(predicate: A => Boolean): Env[E, A]        =
    Env(ctx => run(ctx).flatMap(a => if (predicate(a)) Task.pure(a) else Task.raiseError(error(ctx))))
  def ensureOrCtx(error: (E, A) => Throwable)(predicate: A => Boolean): Env[E, A] =
    Env(ctx => run(ctx).flatMap(a => if (predicate(a)) Task.pure(a) else Task.raiseError(error(ctx, a))))

  def memoizeTask: Env[E, A]          = mapTask(_.memoize)
  def memoizeTaskOnSuccess: Env[E, A] = mapTask(_.memoizeOnSuccess)

  @UnsafeBecauseImpure
  def memoizeOnSuccess: Env[E, A] = {
    val memoization = Memoization.unsafe[A]()

    Env(ctx => memoization.flatMap(_.getOrElse(run(ctx))))
  }

  @UnsafeBecauseImpure
  def memoize: Env[E, A] = {
    val memoization = Memoization.unsafe[Try[A]]()
    Env(ctx => memoization.flatMap(_.getOrElse(run(ctx).materialize)).dematerialize)
  }

  def memoSuccess: Env[E, Env[E, A]] =
    Env.fromTask(Memoization[A].map { memo => Env(ctx => memo.flatMap(_.getOrElse(run(ctx)))) })

  def memo: Env[E, Env[E, A]] =
    Env.fromTask(Memoization[Try[A]].map { memo =>
      Env(ctx => memo.flatMap(_.getOrElse(run(ctx).materialize)).dematerialize)
    })

  // scheduling
  def delayExcecution(dur: FiniteDuration): Env[E, A]                        =
    mapTask(_.delayExecution(dur))
  def delayResult(dur: FiniteDuration): Env[E, A]                            = mapTask(_.delayResult(dur))
  def timeout(dur: FiniteDuration): Env[E, A]                                = mapTask(_.timeout(dur))
  def timeoutToTask[B >: A](dur: FiniteDuration, backup: Task[B]): Env[E, B] =
    mapTask(_.timeoutTo(dur, backup))
  def timeoutTo[B >: A](dur: FiniteDuration, backup: Env[E, B]): Env[E, B]   =
    mapTask2(backup)(_.timeoutTo(dur, _))

  /** Times the Env execution, returning its duration and the computed value in case of success.
    * Delegates to underlying [[monix.eval.Task.timed]].
    * Usage example:
    * {{{
    *   for {
    *     r <- Env.delay(1 + 1).timed
    *     (duration, value) = r
    *     _ <- Env.delay(println("executed in " + duration.toMillis + " ms"))
    *   } yield value
    * }}}
    */
  def timed: Env[E, (FiniteDuration, A)] =
    mapTask(_.timed)

  // concurrency
  def doOnCancel(callback: Env[E, Unit]): Env[E, A] =
    Env(ctx => run(ctx).doOnCancel(callback.run(ctx)))
  def doOnCancelT(callback: Task[Unit]): Env[E, A]  =
    mapTask(_.doOnCancel(callback))
  def onCancelRaiseError(e: Throwable): Env[E, A]   =
    mapTask(_.onCancelRaiseError(e))

  def start: Env[E, Fiber[Env[E, *], A @uv]] = mapTask(_.start.map(EnvFiber(_)))

  def bracket[B](use: A => Env[E, B])(release: A => Env[E, Unit]): Env[E, B]                                  =
    Env(ctx => run(ctx).bracket(a => use(a).run(ctx))(a => release(a).run(ctx)))
  def bracketE[B](use: A => Env[E, B])(release: (A, Either[Option[Throwable], B]) => Env[E, Unit]): Env[E, B] =
    Env(ctx => run(ctx).bracketE(a => use(a).run(ctx))((a, cas) => release(a, cas).run(ctx)))
  def bracketСase[B](use: A => Env[E, B])(release: (A, ExitCase[Throwable]) => Env[E, Unit]): Env[E, B]       =
    Env(ctx => run(ctx).bracketE(a => use(a).run(ctx))((a, cas) => release(a, Env.convertExitCase(cas)).run(ctx)))
  def doOnFinish(f: Option[Throwable] => Env[E, Unit]): Env[E, A]                                             =
    Env(ctx => run(ctx).doOnFinish(oerr => f(oerr).run(ctx)))

  def cancelable: Env[E, A]   = mapTask(_.executeWithOptions(_.enableAutoCancelableRunLoops))
  def uncancelable: Env[E, A] = mapTask(_.uncancelable)

  def asyncBoundary: Env[E, A]                                       = mapTask(_.asyncBoundary)
  def asyncBoundary(s: Scheduler): Env[E, A]                         = mapTask(_.asyncBoundary(s))
  def executeAsync: Env[E, A]                                        = mapTask(_.executeAsync)
  def executeOn(s: Scheduler, forceAsync: Boolean = true): Env[E, A] =
    mapTask(_.executeOn(s, forceAsync))
  def fork: Env[E, Fiber[Env[E, *], A @uv]]                          = executeAsync.start
  @scala.deprecated("Replaced with startAndForget", "0.3.0")
  def forkAndForget: Env[E, Unit]                                    = mapTask(_.startAndForget)
  def startAndForget: Env[E, Unit]                                   = mapTask(_.startAndForget)

  //profunctorial syntax
  def choose[B, C](g: Env[B, C]): Env[Either[E, B], Either[A, C]] =
    Env {
      case Left(ctx) => run(ctx).map(Left(_))
      case Right(b)  => g.run(b).map(Right(_))
    }
  def first[B]: Env[(E, B), (A, B)]                               =
    Env { case (e, b) => run(e).map((_, b)) }
  def second[B]: Env[(B, E), (B, A)]                              =
    Env { case (b, e) => run(e).map((b, _)) }
  def dimap[B, C](f: B => E)(g: A => C): Env[B, C]                = map(g).localP(f)
  def split[B, C](g: Env[B, C]): Env[(E, B), (A, C)]              =
    Env { case (a, c) => Task.parZip2(run(a), g.run(c)) }
  def left[B]: Env[Either[E, B], Either[A, B]]                    = choose(Env.context)
  def right[B]: Env[Either[B, E], Either[B, A]]                   = Env.context.choose(this)
  def choice[E1](f: Env[E1, A @uv]): Env[Either[E, E1], A]        =
    Env {
      case Left(e)   => run(e)
      case Right(e1) => f.run(e1)
    }

  def toReaderT: ReaderT[Task, E, A @uv] = ReaderT(run)
}

/** Context independent variation of `Env` */
final case class EnvTask[E, +A](ta: Task[A]) extends Env[E, A] {
  def run(ctx: E)                                                                         = ta
  override def mapTask[B](f: Task[A] => Task[B]): Env[E, B]                               = EnvTask(f(ta))
  override def mapTask2[B, C](eb: Env[E, B])(f: (Task[A], Task[B]) => Task[C]): Env[E, C] =
    eb match {
      case EnvTask(tb) => EnvTask(f(ta, tb))
      case _           => super.mapTask2(eb)(f)
    }
  override def mapTask3[B1, B2, D](e1: Env[E, B1], e2: Env[E, B2])(
      f: (Task[A], Task[B1], Task[B2]) => Task[D]
  ): Env[E, D]                                                                            =
    e1.mapTask2(e2)(f(ta, _, _))

  override def mapTask4[B1, B2, B3, R](e1: Env[E, B1], e2: Env[E, B2], e3: Env[E, B3])(
      f: (Task[A], Task[B1], Task[B2], Task[B3]) => Task[R]
  ): Env[E, R] =
    e1.mapTask3(e2, e3)(f(ta, _, _, _))

  override def mapTask5[B1, B2, B3, B4, R](e1: Env[E, B1], e2: Env[E, B2], e3: Env[E, B3], e4: Env[E, B4])(
      f: (Task[A], Task[B1], Task[B2], Task[B3], Task[B4]) => Task[R]
  ): Env[E, R] =
    e1.mapTask4(e2, e3, e4)(f(ta, _, _, _, _))

  override def mapTask6[B1, B2, B3, B4, B5, R](
      e1: Env[E, B1],
      e2: Env[E, B2],
      e3: Env[E, B3],
      e4: Env[E, B4],
      e5: Env[E, B5]
  )(f: (Task[A], Task[B1], Task[B2], Task[B3], Task[B4], Task[B5]) => Task[R]): Env[E, R] =
    e1.mapTask5(e2, e3, e4, e5)(f(ta, _, _, _, _, _))

  override def map2[B, C](eb: Env[E, B])(f: (A, B) => C) = eb match {
    case EnvTask(tb) => EnvTask(for (a <- ta; b <- tb) yield f(a, b))
    case _           => super.map2(eb)(f)
  }

  override def memoizeOnSuccess: Env[E, A]                    = EnvTask(ta.memoizeOnSuccess)
  override def memoize: Env[E, A]                             = EnvTask(ta.memoize)
  override def localP[E1](f: E1 => E): Env[E1, A]             =
    this.asInstanceOf[Env[E1, A]]
  override def compose[B](g: Env[B, E]): Env[B, A]            = g match {
    case EnvTask(te) => EnvTask(te.flatMap(_ => ta))
    case _           => super.compose(g)
  }
  override def split[B, C](g: Env[B, C]): Env[(E, B), (A, C)] =
    g match {
      case EnvTask(tc) => EnvTask(Task.parZip2(ta, tc))
      case _           => super.split(g)
    }
}

/** Context aware variation of `Env` */
final case class EnvCtx[E, +A](runF: E => Task[A]) extends Env[E, A] {
  def run(ctx: E) = Task.defer(runF(ctx))
}

object Env extends EnvInstances with EnvFunctions {
  final implicit def preventDelegation[E]: IsTofu[Env[E, *]] = IsTofu

  private def convertExitCase(ec: Either[Option[Throwable], Any]): ExitCase[Throwable] = ec match {
    case Right(_)        => ExitCase.Completed
    case Left(None)      => ExitCase.Canceled
    case Left(Some(err)) => ExitCase.Error(err)
  }
}

trait ProvideEnv[E] {
  def provide: Task[E]
}
