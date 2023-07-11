package tofu.data

import cats.data.IndexedState
import cats.{MonadError, Monoid, StackSafeMonad}
import glass.PContains

import scala.annotation.tailrec
import tofu.Guarantee

sealed trait Calc[-R, -S1, +S2, +E, +A] {
  final def run(r: R, init: S1): (S2, Either[E, A]) = Calc.run(this, r, init)

  final def runUnit(init: S1)(implicit ev: Unit <:< R)  = run((), init)
  final def narrowRead[R1 <: R]: Calc[R1, S1, S2, E, A] = this

  final def exec(r: R, init: S1): S2                        = run(r, init)._1
  final def execUnit(init: S1)(implicit ev: Unit <:< R): S2 = exec((), init)
}

object Calc {
  def unit[S]: Calc[Any, S, S, Nothing, Unit]                       = Pure(())
  def pure[S, A](a: A): Calc[Any, S, S, Nothing, A]                 = Pure(a)
  def read[S, R]: Calc[R, S, S, Nothing, R]                         = Read()
  def get[S]: Calc[Any, S, S, Nothing, S]                           = Get()
  def set[S](s: S): Calc[Any, Any, S, Nothing, Unit]                = Set(s)
  def update[S1, S2](f: S1 => S2): Calc[Any, S1, S2, Nothing, Unit] =
    get[S1].flatMapS(s => set(f(s)))
  def raise[S, E](e: E): Calc[Any, S, S, E, Nothing]                = Raise(e)
  def defer[R, S1, S2, E, A](x: => Calc[R, S1, S2, E, A])           = Defer(() => x)
  def delay[S, A](x: => A): Calc[Any, S, S, Nothing, A]             = defer(pure(x))

  def write[S](s: S)(implicit S: Monoid[S]): Calc[Any, S, S, Nothing, Unit] = update(S.combine(_, s))

  sealed trait CalcRes[-R, -S1, +S2, +E, +A]                              extends Calc[R, S1, S2, E, A]               {
    def submit[X](r: R, s: S1, ke: (S2, E) => X, ka: (S2, A) => X): X
  }
  final case class Pure[S, +A](a: A)                                      extends CalcRes[Any, S, S, Nothing, A]      {
    def submit[X](r: Any, s: S, ke: (S, Nothing) => X, ka: (S, A) => X): X = ka(s, a)
  }
  final case class Read[S, R]()                                           extends CalcRes[R, S, S, Nothing, R]        {
    def submit[X](r: R, s: S, ke: (S, Nothing) => X, ka: (S, R) => X): X = ka(s, r)
  }
  final case class Get[S]()                                               extends CalcRes[Any, S, S, Nothing, S]      {
    def submit[X](r: Any, s: S, ke: (S, Nothing) => X, ka: (S, S) => X): X = ka(s, s)
  }
  final case class Set[S](s: S)                                           extends CalcRes[Any, Any, S, Nothing, Unit] {
    def submit[X](r: Any, s1: Any, ke: (S, Nothing) => X, ka: (S, Unit) => X): X = ka(s, ())
  }
  final case class Raise[S, E](e: E)                                      extends CalcRes[Any, S, S, E, Nothing]      {
    def submit[X](r: Any, s: S, ke: (S, E) => X, ka: (S, Nothing) => X): X = ke(s, e)
  }
  final case class Defer[R, S1, S2, E, A](e: () => Calc[R, S1, S2, E, A]) extends Calc[R, S1, S2, E, A]
  final case class Cont[R, S1, S2, S3, E1, E2, A, B](
      src: Calc[R, S1, S2, E1, A],
      ksuc: A => Calc[R, S2, S3, E2, B],
      kerr: E1 => Calc[R, S2, S3, E2, B]
  ) extends Calc[R, S1, S3, E2, B] {
    type MidState = S2
    type MidErr   = E1
  }

  implicit final class invariantOps[R, S1, S2, E, A](private val calc: Calc[R, S1, S2, E, A]) extends AnyVal {
    def cont[R1 <: R, E2, S3, B](
        f: A => Calc[R1, S2, S3, E2, B],
        h: E => Calc[R1, S2, S3, E2, B]
    ): Calc[R1, S1, S3, E2, B] = Cont(calc, f, h)
    def flatMap[R1 <: R, E1 >: E, B](f: A => Calc[R1, S2, S2, E1, B]): Calc[R1, S1, S2, E1, B] = cont(f, raise(_: E))
    def >>=[R1 <: R, E1 >: E, B](f: A => Calc[R1, S2, S2, E1, B])                              = flatMap(f)
    def >>[R1 <: R, E1 >: E, B](c: => Calc[R1, S2, S2, E1, B])                                 = flatMap(_ => c)
    def handleWith[E1](f: E => Calc[R, S2, S2, E1, A]): Calc[R, S1, S2, E1, A]                 = cont(pure(_: A), f)
    def handle(f: E => A): Calc[R, S1, S2, E, A]                                               = handleWith(e => pure(f(e)))
    def map[B](f: A => B): Calc[R, S1, S2, E, B]                                               = flatMap(a => pure(f(a)))
    def as[B](b: => B): Calc[R, S1, S2, E, B]                                                  = map(_ => b)
    def mapError[E1](f: E => E1): Calc[R, S1, S2, E1, A]                                       = handleWith(e => Calc.raise(f(e)))

    def focus[S3, S4](lens: PContains[S3, S4, S1, S2]): Calc[R, S3, S4, E, A] =
      get[S3].flatMapS { s3 =>
        set(lens.extract(s3)) *>> calc.cont(
          result => get[S2].flatMapS(s2 => set(lens.set(s3, s2)) *>> pure(result)),
          err => get[S2].flatMapS(s2 => set(lens.set(s3, s2)) *>> raise(err))
        )
      }
  }

  implicit final class CalcSuccessfullOps[R, S1, S2, A](private val calc: Calc[R, S1, S2, Nothing, A]) extends AnyVal {
    def flatMapS[R1 <: R, S3, B, E](f: A => Calc[R1, S2, S3, E, B]): Calc[R1, S1, S3, E, B] =
      calc.cont[R1, E, S3, B](f, (e: Nothing) => e)
    def productRS[R1 <: R, S3, B, E](r: => Calc[R1, S2, S3, E, B]): Calc[R1, S1, S3, E, B]  = flatMapS(_ => r)
    def *>>[R1 <: R, S3, B, E](r: => Calc[R1, S2, S3, E, B]): Calc[R1, S1, S3, E, B]        = productRS(r)
    def runSuccess(r: R, init: S1): (S2, A)                                                 = {
      val (s2, res) = calc.run(r, init)
      s2 -> res.merge
    }
  }

  implicit final class CalcUnsuccessfullOps[R, S1, S2, E](private val calc: Calc[R, S1, S2, E, Nothing])
      extends AnyVal {
    def handleWithS[R1 <: R, E1, S3, B, A](f: E => Calc[R, S2, S3, E1, A]): Calc[R1, S1, S3, E1, A] =
      calc.cont((a: Calc[R1, S2, S3, E1, A]) => a, f)
  }

  implicit final class CalcFixedStateOps[R, S, E, A](private val calc: Calc[R, S, S, E, A]) extends AnyVal {
    def when(b: Boolean): Calc[R, S, S, E, Any]                                = if (b) calc else Calc.unit
    def runEmpty(r: R)(implicit S: Monoid[S]): (S, Either[E, A])               = calc.run(r, S.empty)
    def runUnitEmpty(implicit S: Monoid[S], ev: Unit <:< R): (S, Either[E, A]) = calc.runUnit(S.empty)
    def execEmpty(r: R)(implicit S: Monoid[S]): S                              = calc.exec(r, S.empty)
    def execUnitEmpty(implicit S: Monoid[S], ev: Unit <:< R): S                = calc.exec((), S.empty)
  }

  implicit final class CalcFixedStateSuccessOps[R, S, A](private val clc: Calc[R, S, S, Nothing, A]) extends AnyVal {
    def calc(r: R, s: S): (S, A) = clc.run(r, s) match { case (a, b) => a -> b.merge }

    def calcEmpty(r: R)(implicit S: Monoid[S]): (S, A)               = clc.calc(r, S.empty)
    def calcUnit(s: S)(implicit ev: Unit <:< R): (S, A)              = clc.calc((), s)
    def calcUnitEmpty(implicit S: Monoid[S], ev: Unit <:< R): (S, A) = clc.calcUnit(S.empty)
  }

  implicit final class CalcFixedStateUnsuccessOps[R, S, E](private val clc: Calc[R, S, S, E, Nothing]) extends AnyVal {
    def error(r: R, s: S): (S, E) = clc.run(r, s) match { case (a, b) => a -> b.merge }

    def errorEmpty(r: R)(implicit S: Monoid[S])                       = clc.error(r, S.empty)
    def errorUnit(s: S)(implicit ev: Unit <:< R): (S, E)              = clc.error((), s)
    def errorUnitEmpty(implicit S: Monoid[S], ev: Unit <:< R): (S, E) = clc.errorUnit(S.empty)
  }

  implicit class CalcSimpleStateOps[S1, S2, A](private val calc: Calc[Any, S1, S2, Nothing, A]) extends AnyVal {
    final def runSuccessUnit(init: S1): (S2, A) = calc.runSuccess((), init)

    def toState: IndexedState[S1, S2, A] = IndexedState(runSuccessUnit)
  }
  
  @tailrec def run[R, S1, S2, S3, E1, E2, A](calc: Calc[R, S1, S3, E2, A], r: R, init: S1): (S3, Either[E2, A]) =
    calc match {
      case res: CalcRes[R, S1, S3, E2, A]        =>
        res.submit(r, init, (s2, e) => (s2, Left(e)), (s2, a) => (s2, Right(a)))
      case Defer(f)                              => run(f(), r, init)
      case c: Cont[R, S1, S2, S3, E1, E2, S2, A] =>
        c.src match {
          case res: CalcRes[R, S1, c.MidState, c.MidErr, c.MidState] =>
            val (sm, next) =
              res.submit[(c.MidState, Calc[R, c.MidState, S3, E2, A])](
                r,
                init,
                (sm, e) => (sm, c.kerr(e)),
                (sm, a) => (sm, c.ksuc(a))
              )
            run(next, r, sm)
          case Defer(f)                                              => run(f().cont(c.ksuc, c.kerr), r, init)
          case Cont(src1, ks1, ke1)                                  =>
            run(src1.cont(a => ks1(a).cont(c.ksuc, c.kerr), e => ke1(e).cont(c.ksuc, c.kerr)), r, init)
        }
    }

  implicit def calcInstance[R, S, E]: CalcFunctorInstance[R, S, E] = new CalcFunctorInstance[R, S, E]

  class CalcFunctorInstance[R, S, E]
      extends MonadError[Calc[R, S, S, E, *], E] with cats.Defer[Calc[R, S, S, E, *]]
      with StackSafeMonad[Calc[R, S, S, E, *]] with Guarantee[Calc[R, S, S, E, *]] {
    def defer[A](fa: => Calc[R, S, S, E, A]): Calc[R, S, S, E, A]                                     = Calc.defer(fa)
    def raiseError[A](e: E): Calc[R, S, S, E, A]                                                      = Calc.raise(e)
    def handleErrorWith[A](fa: Calc[R, S, S, E, A])(f: E => Calc[R, S, S, E, A]): Calc[R, S, S, E, A] = fa.handleWith(f)
    def flatMap[A, B](fa: Calc[R, S, S, E, A])(f: A => Calc[R, S, S, E, B]): Calc[R, S, S, E, B]      = fa.flatMap(f)
    def pure[A](x: A): Calc[R, S, S, E, A]                                                            = Calc.pure(x)

    def bracket[A, B, C](acquire: Calc[R, S, S, E, A])(use: A => Calc[R, S, S, E, B])(
        release: (A, Boolean) => Calc[R, S, S, E, C]
    ): Calc[R, S, S, E, B] =
      acquire.flatMap { a =>
        use(a).cont(
          b => release(a, true).as(b),
          e => release(a, false) >> Calc.raise(e)
        )
      }
  }
}
