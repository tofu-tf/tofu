package tofu.data

import scala.annotation.unchecked.{uncheckedVariance => uv}
import CalcMSpecials._
import cats.data.IndexedState
import cats.evidence.Is
import cats.{Functor, Monad, MonadError, Monoid, StackSafeMonad}
import tofu.optics.PContains
import cats.~>

import scala.annotation.tailrec
import cats.evidence.As

sealed trait CalcM[+F[+_], -R, -S1, +S2, +E, +A] {
  def narrowRead[R1 <: R]: CalcM[F, R1, S1, S2, E, A] = this
  def mapK[G[+_]](fk: F FunK G): CalcM[G, R, S1, S2, E, A]
}

object CalcM {
  def unit[S]: CalcM[Nothing, Any, S, S, Nothing, Unit]                       = Pure(())
  def pure[S, A](a: A): CalcM[Nothing, Any, S, S, Nothing, A]                 = Pure(a)
  def read[S, R]: CalcM[Nothing, R, S, S, Nothing, R]                         = Read()
  def get[S]: CalcM[Nothing, Any, S, S, Nothing, S]                           = Get()
  def set[S](s: S): CalcM[Nothing, Any, Any, S, Nothing, Unit]                = Set(s)
  def update[S1, S2](f: S1 => S2): CalcM[Nothing, Any, S1, S2, Nothing, Unit] =
    get[S1].flatMapS(s => set(f(s)))
  def raise[S, E](e: E): CalcM[Nothing, Any, S, S, E, Nothing]                = Raise(e)
  def defer[F[+_], R, S1, S2, E, A](x: => CalcM[F, R, S1, S2, E, A])          = Defer(() => x)
  def delay[S, A](x: => A): CalcM[Nothing, Any, S, S, Nothing, A]             = defer(pure[S, A](x))

  def write[S](s: S)(implicit S: Monoid[S]): CalcM[Nothing, Any, S, S, Nothing, Unit] = update(S.combine(_, s))

  sealed trait CalcMRes[-R, -S1, +S2, +E, +A] extends CalcM[Nothing, R, S1, S2, E, A]      {
    def submit[X](submit: Submit[R, S1, S2, E, A, X]): X
    def mapK[G[+_]](fk: Nothing ~> G): CalcM[G, R, S1, S2, E, A] = this
  }
  final case class Pure[S, +A](a: A)          extends CalcMRes[Any, S, S, Nothing, A]      {
    def submit[X](submit: Submit[Any, S, S, Nothing, A, X]): X = submit.success(submit.state, a)
  }
  final case class Read[S, R]()               extends CalcMRes[R, S, S, Nothing, R]        {
    def submit[X](submit: Submit[R, S, S, Nothing, R, X]): X = submit.success(submit.state, submit.read)
  }
  final case class Get[S]()                   extends CalcMRes[Any, S, S, Nothing, S]      {
    def submit[X](submit: Submit[Any, S, S, Nothing, S, X]): X = submit.success(submit.state, submit.state)
  }
  final case class Set[S](s: S)               extends CalcMRes[Any, Any, S, Nothing, Unit] {
    def submit[X](submit: Submit[Any, Any, S, Nothing, Unit, X]): X = submit.success(s, ())
  }
  final case class Raise[S, E](e: E)          extends CalcMRes[Any, S, S, E, Nothing]      {
    def submit[X](submit: Submit[Any, S, S, E, Nothing, X]): X = submit.error(submit.state, e)
  }
  final case class Defer[+F[+_], -R, -S1, +S2, +E, +A](runStep: () => CalcM[F, R, S1, S2, E, A])
      extends CalcM[F, R, S1, S2, E, A] {
    def mapK[G[+_]](fk: F FunK G): CalcM[G, R, S1, S2, E, A] = Defer(() => runStep().mapK(fk))
  }

  sealed trait ProvideM[+F[+_], R, -S1, +S2, +E, +A] extends CalcM[F, R, S1, S2, E, A] {
    type R1
    def r: R1
    def inner: CalcM[F, R1, S1, S2, E, A]
    def any: R Is Any
  }

  final case class Provide[+F[+_], R, -S1, +S2, +E, +A](r: R, inner: CalcM[F, R, S1, S2, E, A])
      extends ProvideM[F, Any, S1, S2, E, A] {
    type R1 = R
    def any                                                    = Is.refl
    def mapK[G[+_]](fk: F FunK G): CalcM[G, Any, S1, S2, E, A] = Provide(r, inner.mapK(fk))
  }

  abstract class Sub[+F[+_], -S1, +S2, A](val fa: F[A]) extends CalcM[F, Any, S1, S2, Nothing, A] {
    def iss: S1 As S2
    def subcalc[F1[+_], R, SX, E, A1](calc: CalcM[F1, R, S2, SX, E, A1]): CalcM[F1, R, S1, SX, E, A1] =
      iss.substitute[CalcM[F1, R, -*, SX, E, A1]](calc)
    final def mapK[G[+_]](fk: F FunK G): CalcM[G, Any, S1, S2, Nothing, A]                            =
      iss.substitute[CalcM[G, Any, -*, S2, Nothing, A] @uv](Sub(fk(fa)))
  }

  object Sub {
    def apply[F[+_], S, A](fa: F[A]): CalcM[F, Any, S, S, Nothing, A] = new Sub[F, S, S, A](fa) {
      def iss = As.refl
    }
  }

  final case class Bind[+F[+_], R, S1, S2, S3, E1, E2, A, B](
      src: CalcM[F, R, S1, S2, E1, A],
      continue: Continue[A, E1, CalcM[F, R, S2, S3, E2, B]],
  ) extends CalcM[F, R, S1, S3, E2, B] {
    type MidState = S2
    type MidErr   = E1
    type MidVal   = A

    def mapK[G[+_]](fk: F FunK G): CalcM[G, R, S1, S3, E2, B] =
      Bind(
        src.mapK(fk),
        new Continue[A, E1, CalcM[G, R, S2, S3, E2, B]] {
          def success(a: A): CalcM[G, R, S2, S3, E2, B] = continue.success(a).mapK(fk)
          def error(e: E1): CalcM[G, R, S2, S3, E2, B]  = continue.error(e).mapK(fk)
        }
      )
  }

  implicit class invariantOps[F[+_], R, S1, S2, E, A](private val calc: CalcM[F, R, S1, S2, E, A]) extends AnyVal {
    final def step(r: R, init: S1)(implicit F: Functor[F]): StepResult[F, S2, E, A] =
      CalcM.step(calc, r, init)

    final def runTailRec(r: R, init: S1)(implicit F: Monad[F]): F[(S2, Either[E, A])] =
      F.tailRecM(calc.provideSet(r, init)) { c =>
        c.step((), ()) match {
          case now: StepResult.Now[S2, E, A]               => F.pure(Right((now.state, now.result)))
          case wrap: StepResult.Wrap[F, r, s, S2, E, m, A] => F.map(wrap.provided(F))(Left(_))
        }
      }

    final def runUnit(init: S1)(implicit ev: Unit <:< R, F: Functor[F]): StepResult[F, S2, E, A] = step((), init)

    def bind[R1 <: R, E2, S3, B](continue: Continue[A, E, CalcM[F, R1, S2, S3, E2, B]]): CalcM[F, R1, S1, S3, E2, B] =
      Bind(calc, continue)
    def flatMap[R1 <: R, E1 >: E, B](f: A => CalcM[F, R1, S2, S2, E1, B]): CalcM[F, R1, S1, S2, E1, B]               =
      bind(Continue.flatMapConst[A, E, S2, CalcM[F, R1, S2, S2, E1, B]](f))
    def >>=[R1 <: R, E1 >: E, B](f: A => CalcM[F, R1, S2, S2, E1, B])                                                = flatMap(f)
    def >>[R1 <: R, E1 >: E, B](c: => CalcM[F, R1, S2, S2, E1, B])                                                   = flatMap(_ => c)
    def handleWith[E1](f: E => CalcM[F, R, S2, S2, E1, A]): CalcM[F, R, S1, S2, E1, A]                               =
      bind(Continue.handleWithConst[A, E, S2, CalcM[F, R, S2, S2, E1, A]](f))
    def handle(f: E => A): CalcM[F, R, S1, S2, E, A]                                                                 = handleWith(e => pure(f(e)))
    def map[B](f: A => B): CalcM[F, R, S1, S2, E, B]                                                                 = flatMap(a => pure(f(a)))
    def as[B](b: => B): CalcM[F, R, S1, S2, E, B]                                                                    = map(_ => b)
    def mapError[E1](f: E => E1): CalcM[F, R, S1, S2, E1, A]                                                         = handleWith(e => CalcM.raise(f(e)))
    def provideSet(r: R, s: S1): CalcM[F, Any, Any, S2, E, A]                                                        = set(s) *>> calc.provide(r)
    def provide(r: R): CalcM[F, Any, S1, S2, E, A]                                                                   = Provide(r, calc)
    def provideSome[R1](f: R1 => R): CalcM[F, R1, S1, S2, E, A]                                                      = read[S1, R1] flatMapS (r => calc.provide(f(r)))

    def focus[S3, S4](lens: PContains[S3, S4, S1, S2]): CalcM[F, R, S3, S4, E, A] =
      get[S3].flatMapS { s3 =>
        set(lens.extract(s3)) *>> calc.bind(
          new Continue[A, E, CalcM[F, R, S2, S4, E, A]] {
            def success(result: A): CalcM[F, R, S2, S4, E, A] =
              get[S2].flatMapS(s2 => set(lens.set(s3, s2)) *>> pure(result))
            def error(err: E): CalcM[F, R, S2, S4, E, A]      = get[S2].flatMapS(s2 => set(lens.set(s3, s2)) *>> raise(err))
          }
        )
      }
  }

  implicit class CalcSuccessfullOps[F[+_], R, S1, S2, A](private val calc: CalcM[F, R, S1, S2, Nothing, A])
      extends AnyVal {
    final def flatMapS[R1 <: R, S3, B, E](f: A => CalcM[F, R1, S2, S3, E, B]): CalcM[F, R1, S1, S3, E, B] =
      calc.bind(Continue.flatMapSuccess[A, B, S2, S3, CalcM[F, R1, S2, S3, E, B]](f))
    final def productRS[R1 <: R, S3, B, E](r: => CalcM[F, R1, S2, S3, E, B]): CalcM[F, R1, S1, S3, E, B]  =
      flatMapS(_ => r)
    final def *>>[R1 <: R, S3, B, E](r: => CalcM[F, R1, S2, S3, E, B]): CalcM[F, R1, S1, S3, E, B]        = productRS(r)
  }

  implicit class CalcPureOps[R, S1, S2, E, A](private val calc: CalcM[Nothing, R, S1, S2, E, A]) extends AnyVal {
    final def run(r: R, init: S1): (S2, Either[E, A]) =
      step[Nothing, R, S1, S2, E, A](calc, r, init) match {
        case wrap: StepResult.Wrap[Nothing, _, _, S2, E, m, A] => wrap.inner: Nothing
        case StepResult.Error(s, err)                          => (s, Left(err))
        case StepResult.Ok(s, a)                               => (s, Right(a))
      }
  }

  implicit class CalcPureSuccessfullOps[R, S1, S2, A](private val calc: CalcM[Nothing, R, S1, S2, Nothing, A])
      extends AnyVal {
    final def runSuccess(r: R, init: S1): (S2, A) =
      step[Nothing, R, S1, S2, Nothing, A](calc, r, init) match {
        case wrap: StepResult.Wrap[Nothing, _, _, S2, Nothing, m, A] => wrap.inner: Nothing
        case StepResult.Error(_, err)                                => err
        case StepResult.Ok(s, a)                                     => (s, a)
      }
  }

  implicit class CalcUnsuccessfullOps[F[+_], R, S1, S2, E](private val calc: CalcM[F, R, S1, S2, E, Nothing])
      extends AnyVal {
    def handleWithS[R1 <: R, E1, S3, B, A](f: E => CalcM[F, R, S2, S3, E1, A]): CalcM[F, R1, S1, S3, E1, A] =
      calc.bind(Continue.handleWithFail[E, E1, S2, S3, CalcM[F, R, S2, S3, E1, A]](f))
  }

  implicit class CalcFixedStateOps[F[+_], R, S, E, A](private val calc: CalcM[F, R, S, S, E, A]) extends AnyVal {
    def when(b: Boolean): CalcM[F, R, S, S, E, Any] = if (b) calc else CalcM.unit
  }

  implicit class CalcSimpleStateOps[F[+_], S1, S2, A](private val calc: CalcM[Nothing, Any, S1, S2, Nothing, A])
      extends AnyVal {
    final def runSuccessUnit(init: S1): (S2, A) = calc.runSuccess((), init)

    def toState: IndexedState[S1, S2, A] = IndexedState(runSuccessUnit)
  }

  @tailrec
  def step[F[+_], R, S1, S2, E, A](calc: CalcM[F, R, S1, S2, E, A], r: R, init: S1)(implicit
      F: Functor[F]
  ): StepResult[F, S2, E, A] =
    calc match {
      case res: CalcMRes[R, S1, S2, E, A]           =>
        res.submit(new Submit[R, S1, S2, E, A, StepResult[F, S2, E, A]](r, init) {
          def success(s: S2, a: A) = StepResult.Ok(s, a)
          def error(s: S2, err: E) = StepResult.Error(s, err)
        })
      case d: Defer[F, R, S1, S2, E, A]             => step(d.runStep(), r, init)
      case sub: Sub[F, S1, S2, A]                   =>
        StepResult.Wrap[F, R, S1, S2, E, A, A](
          r,
          init,
          sub.fa,
          (a: A) => sub.iss.substitute[CalcM[F, R, -*, S2, E, A]](CalcM.pure[S2, A](a))
        )
      case p: Provide[F, r, S1, S2, E, A]           => step[F, r, S1, S2, E, A](p.inner, p.r, init)
      case c1: Bind[F, R, S1, s1, S2, e1, E, a1, A] =>
        c1.src match {
          case res: CalcMRes[R, S1, c1.MidState, e1, a1] =>
            val (sm, next) =
              res.submit(new Submit[R, S1, s1, e1, a1, (s1, CalcM[F, R, s1, S2, E, A])](r, init) {
                def success(s: s1, a: a1) = (s, c1.continue.success(a))
                def error(s: s1, err: e1) = (s, c1.continue.error(err))
              })
            step[F, R, s1, S2, E, A](next, r, sm)
          case d: Defer[F, R, S1, _, _, _]               => step(d.runStep().bind(c1.continue), r, init)
          case sub: Sub[F, S1, s1, c1.MidVal]            =>
            StepResult
              .Wrap[F, R, S1, S2, E, a1, A](r, init, sub.fa, a => sub.subcalc[F, R, S2, E, A](c1.continue.success(a)))
          case p: ProvideM[F, R, S1, _, _, _]            =>
            val kcont = p.any.substitute[λ[r => Continue[a1, e1, CalcM[F, r, s1, S2, E, A]]]](c1.continue)

            step(p.inner.bind[p.R1, E, S2, A](kcont), p.r, init)
          case c2: Bind[F, R, S1, s2, _, e2, _, a2, _]   =>
            step(c2.src.bind(Continue.compose(c2.continue, c1.continue)), r, init)
        }
    }

  implicit def calcInstance[F[+_], R, S, E]: CalcFunctorInstance[F, R, S, E] = new CalcFunctorInstance[F, R, S, E]

  class CalcFunctorInstance[F[+_], R, S, E]
      extends MonadError[CalcM[F, R, S, S, E, *], E] with cats.Defer[CalcM[F, R, S, S, E, *]]
      with StackSafeMonad[CalcM[F, R, S, S, E, *]] {
    def defer[A](fa: => CalcM[F, R, S, S, E, A]): CalcM[F, R, S, S, E, A]                                         = CalcM.defer(fa)
    def raiseError[A](e: E): CalcM[F, R, S, S, E, A]                                                              = CalcM.raise(e)
    def handleErrorWith[A](fa: CalcM[F, R, S, S, E, A])(f: E => CalcM[F, R, S, S, E, A]): CalcM[F, R, S, S, E, A] =
      fa.handleWith(f)
    def flatMap[A, B](fa: CalcM[F, R, S, S, E, A])(f: A => CalcM[F, R, S, S, E, B]): CalcM[F, R, S, S, E, B]      =
      fa.flatMap(f)
    def pure[A](x: A): CalcM[F, R, S, S, E, A]                                                                    = CalcM.pure(x)
  }
}

object CalcMSpecials {
  trait Continue[-A, -E, +C] {
    def success(a: A): C
    def error(e: E): C
  }

  object Continue {
    def compose[A, B, C, E, V, W, R, S1, S2, S3, F[+_]](
        c1: Continue[A, E, CalcM[F, R, S1, S2, V, B]],
        c2: Continue[B, V, CalcM[F, R, S2, S3, W, C]]
    ): Continue[A, E, CalcM[F, R, S1, S3, W, C]] =
      new Continue[A, E, CalcM[F, R, S1, S3, W, C]] {
        def success(a: A): CalcM[F, R, S1, S3, W, C] = c1.success(a).bind(c2)
        def error(e: E): CalcM[F, R, S1, S3, W, C]   = c1.error(e).bind(c2)
      }

    def flatMapConst[A, E, S, X >: CalcM[Nothing, Any, S, S, E, Nothing]](f: A => X): Continue[A, E, X] =
      new Continue[A, E, X] {
        def success(a: A): X = f(a)
        def error(e: E): X   = CalcM.Raise[S, E](e)
      }

    def handleWithConst[A, E, S, X >: CalcM[Nothing, Any, S, S, Nothing, A]](f: E => X): Continue[A, E, X] =
      new Continue[A, E, X] {
        def success(a: A): X = CalcM.Pure[S, A](a)
        def error(e: E): X   = f(e)
      }

    def flatMapSuccess[A, B, S1, S2, X >: CalcM[Nothing, Any, S1, S2, Nothing, B]](f: A => X): Continue[A, Nothing, X] =
      new Continue[A, Nothing, X] {
        def success(a: A): X     = f(a)
        def error(e: Nothing): X = e
      }

    def handleWithFail[E, V, S1, S2, X >: CalcM[Nothing, Any, S1, S2, V, Nothing]](f: E => X): Continue[Nothing, E, X] =
      new Continue[Nothing, E, X] {
        def success(a: Nothing): X = a
        def error(e: E): X         = f(e)
      }
  }

  sealed trait StepResult[+F[+_], +S, +E, +A]

  object StepResult {
    sealed trait Now[+S, +E, +A] extends StepResult[Nothing, S, E, A] {
      def state: S
      def result: Either[E, A] = this match {
        case Ok(_, a)    => Right(a)
        case Error(_, e) => Left(e)
      }
    }

    final case class Ok[+S, +A](state: S, value: A)    extends Now[S, Nothing, A]
    final case class Error[+S, +E](state: S, error: E) extends Now[S, E, Nothing]
    final case class Wrap[+F[+_], R, S1, +S2, +E, M, +A](
        input: R,
        state: S1,
        inner: F[M],
        k: M => CalcM[F, R, S1, S2, E, A]
    ) extends StepResult[F, S2, E, A] {
      def provided[F1[+a] >: F[a] @uv](implicit F: Functor[F1]): F1[CalcM[F1, Any, Any, S2, E, A]] =
        F.map(inner)(m => k(m).provideSet(input, state))
    }
  }

  abstract class Submit[+R, +S1, -S2, -E, -A, +X](val read: R, val state: S1) {
    def success(s: S2, a: A): X
    def error(s: S2, err: E): X
  }
}
