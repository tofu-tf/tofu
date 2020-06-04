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
import tofu.control.StackSafeBind
import tofu.WithRun
import tofu.syntax.funk._

sealed trait CalcM[+F[+_], -R, -SI, +SO, +E, +A] {
  import CalcM.{Pure, Raise, Provide, Set, Get, Bind}

  def narrowRead[R1 <: R]: CalcM[F, R1, SI, SO, E, A] = this

  def mapK[G[+_]](fk: F FunK G): CalcM[G, R, SI, SO, E, A]

  def local[R1](f: R1 => R): CalcM[F, R1, SI, SO, E, A]

  def widenF[F1[+x] >: F[x]]: CalcM[F1, R, SI, SO, E, A] = this

  def hyperFlatMap[G[+_], R1, SI1, SO1, E1](
      fk: F FunK CalcM[G, R1, SI1, SO1, E1, *],
      fr: R1 => R,
      fsi: SI1 => SI,
      fso: SO => SO1,
      fe: E => E1
  ): CalcM[G, R1, SI1, SO1, E1, A] = ???

  def contramapState[SI1](f: SI1 => SI): CalcM[F, R, SI1, SO, E, A] = CalcM.update(f) *>> this
  def mapState[SO1](f: SO => SO1): CalcM[F, R, SI, SO1, E, A]       =
    bind(Continue.update(f))

  def dimapState[SI1, SO1](f: SI1 => SI, g: SO => SO1): CalcM[F, R, SI1, SO1, E, A] =
    contramapState(f).mapState(g)

  def flatMapLocalK[G[+_], R1, SI1 <: SI, SO1 >: SO, E1 >: E](
      fk: F FunK CalcM[G, R1, SI1, SO1, E1, *],
      f: R1 => R,
  ): CalcM[G, R1, SI1, SO1, E1, A] = hyperFlatMap[G, R1, SI1, SO1, E1](fk, f, identity, identity, identity)

  def flatMapK[G[+_], R1 <: R, SI1 <: SI, SO1 >: SO, E1 >: E](
      fk: F FunK CalcM[G, R1, SI1, SO1, E1, *]
  ): CalcM[G, R1, SI1, SO1, E1, A] = hyperFlatMap[G, R1, SI1, SO1, E1](fk, identity, identity, identity, identity)

  def bind[F1[+x] >: F[x], R1 <: R, X, S, B](
      continue: Continue[A, E, CalcM[F1, R1, SO, S, X, B]]
  ): CalcM[F1, R1, SI, S, X, B] =
    CalcM.Bind(this, continue)

  def foldWith[F1[+x] >: F[x], R1 <: R, X, S, B](
      f: A => CalcM[F1, R1, SO, S, X, B],
      h: E => CalcM[F1, R1, SO, S, X, B]
  ): CalcM[F1, R1, SI, S, X, B] = bind(Continue(f, h))

  def flatMap[F1[+x] >: F[x], R1 <: R, SO1 >: SO, E1 >: E, B](
      f: A => CalcM[F1, R1, SO, SO1, E1, B]
  ): CalcM[F1, R1, SI, SO1, E1, B] =
    bind(Continue.flatMapConst[A, E, SO, CalcM[F1, R1, SO, SO1, E1, B]](f))

  def flatTap[F1[+x] >: F[x], R1 <: R, SO1 >: SO, E1 >: E, B](
      f: A => CalcM[F1, R1, SO, SO1, E1, B]
  ): CalcM[F1, R1, SI, SO1, E1, A] =
    flatMap(a => f(a) as a)

  def >>=[F1[+x] >: F[x], R1 <: R, E1 >: E, SO1 >: SO, B](f: A => CalcM[F1, R1, SO, SO1, E1, B]) = flatMap(f)
  def >>[F1[+x] >: F[x], R1 <: R, E1 >: E, SO1 >: SO, B](c: => CalcM[F1, R1, SO, SO1, E1, B])    = flatMap(_ => c)
  def <<[F1[+x] >: F[x], R1 <: R, E1 >: E, SO1 >: SO, B](
      c: => CalcM[F1, R1, SO, SO1, E1, B]
  ): CalcM[F1, R1, SI, SO1, E1, A]                                                               = flatTap(_ => c)
  def map[B](f: A => B): CalcM[F, R, SI, SO, E, B]                                               = flatMap(a => CalcM.Pure(f(a)))

  def handleWith[F1[+x] >: F[x], E1, R1 <: R, SO1 >: SO, A1 >: A](
      f: E => CalcM[F1, R1, SO, SO1, E1, A1]
  ): CalcM[F1, R1, SI, SO1, E1, A1] =
    bind(Continue.handleWithConst[A, E, SO, CalcM[F1, R1, SO, SO1, E1, A1]](f))

  def handle[A1 >: A](f: E => A1): CalcM[F, R, SI, SO, E, A1] = handleWith(e => CalcM.Pure(f(e)))

  def as[B](b: => B): CalcM[F, R, SI, SO, E, B]            = map(_ => b)
  def as_[B](b: B): CalcM[F, R, SI, SO, E, B]              = map(_ => b)
  def mapError[E1](f: E => E1): CalcM[F, R, SI, SO, E1, A] = handleWith(e => CalcM.raise(f(e)))

  def errorAs[X](e: => X): CalcM[F, R, SI, SO, X, A]        = mapError(_ => e)
  def errorAs_[X](b: X): CalcM[F, R, SI, SO, X, A]          = mapError(_ => b)
  def provideSet(r: R, s: SI): CalcM[F, Any, Any, SO, E, A] = CalcM.Set(s) *>> provide(r)
  def provide(r: R): CalcM[F, Any, SI, SO, E, A]            = CalcM.Provide(r, this)

  def focus[S3, S4](lens: PContains[S3, S4, SI, SO]): CalcM[F, R, S3, S4, E, A] =
    Get[S3].flatMapS { s3 =>
      Set(lens.extract(s3)) *>> this.bind(
        new Continue[A, E, CalcM[F, R, SO, S4, E, A]] {
          def success(result: A): CalcM[F, R, SO, S4, E, A] =
            Get[SO].flatMapS(s2 => Set(lens.set(s3, s2)) *>> Pure(result))
          def error(err: E): CalcM[F, R, SO, S4, E, A]      = Get[SO].flatMapS(s2 => Set(lens.set(s3, s2)) *>> Raise(err))
        }
      )
    }

  def provideSome[R1](f: R1 => R): CalcM[F, R1, SI, SO, E, A] = local(f)

  final def flatMapS[F1[+x] >: F[x], R1 <: R, S, E1, B](
      f: A => CalcM[F1, R1, SO, S, E1, B]
  )(implicit ev: E <:< Nothing): CalcM[F1, R1, SI, S, E1, B] =
    foldWith(f, ev)

  final def flatTapS[F1[+x] >: F[x], R1 <: R, S, E1, B](
      f: A => CalcM[F1, R1, SO, S, E1, B]
  )(implicit ev: E <:< Nothing): CalcM[F1, R1, SI, S, E1, A] =
    foldWith(a => f(a) as_ a, ev)

  final def productRS[F1[+x] >: F[x], R1 <: R, S, B, E1](
      r: => CalcM[F1, R1, SO, S, E1, B]
  )(implicit ev: E <:< Nothing): CalcM[F1, R1, SI, S, E1, B] =
    flatMapS(_ => r)

  final def productLS[F1[+x] >: F[x], R1 <: R, S, B, E1](
      r: => CalcM[F1, R1, SO, S, E1, B]
  )(implicit ev: E <:< Nothing): CalcM[F1, R1, SI, S, E1, A] =
    flatTapS(_ => r)

  def *>>[F1[+x] >: F[x], R1 <: R, S, B, E1](r: => CalcM[F1, R1, SO, S, E1, B])(implicit
      ev: E <:< Nothing
  ): CalcM[F1, R1, SI, S, E1, B] = productRS(r)

  def <<*[F1[+x] >: F[x], R1 <: R, S, B, E1](r: => CalcM[F1, R1, SO, S, E1, B])(implicit
      ev: E <:< Nothing
  ): CalcM[F1, R1, SI, S, E1, A] = productLS(r)

  def handleWithU[F1[+x] >: F[x], R1 <: R, E1, S3, B](
      f: E => CalcM[F1, R1, SO, S3, E1, B]
  )(implicit ev: A <:< Nothing): CalcM[F1, R1, SI, S3, E1, B] =
    foldWith(ev, f)

  def onErrorU[F1[+x] >: F[x], R1 <: R, E1, S3, B](
      f: E => CalcM[F1, R1, SO, S3, E1, B]
  )(implicit ev: A <:< Nothing): CalcM[F1, R1, SI, S3, E, B] =
    foldWith(ev, x => f(x) errorAs_ x)

  def !>>[F1[+x] >: F[x], R1 <: R, E1, S3, B](
      r: => CalcM[F1, R1, SO, S3, E1, B]
  )(implicit ev: A <:< Nothing): CalcM[F1, R1, SI, S3, E1, B] =
    handleWithU(_ => r)

  def <<![F1[+x] >: F[x], R1 <: R, E1, S3, B](
      r: => CalcM[F1, R1, SO, S3, E1, B]
  )(implicit ev: A <:< Nothing): CalcM[F1, R1, SI, S3, E, B] =
    onErrorU(_ => r)

  def swap: CalcM[F, R, SI, SO, A, E] = bind(Continue.swap[A, E, SO, CalcM[Nothing, Any, SO, SO, A, E]])

  def when[S >: SO <: SI](b: Boolean): CalcM[F, R, S, S, E, Any] =
    if (b) this else CalcM.unit[S]

  def step(r: R, init: SI): StepResult[F, SO, E, A] = CalcM.step[F, R, SI, SO, E, A](this, r, init)

  def runTailRec[F1[+x] >: F[x]](r: R, init: SI)(implicit F: Monad[F1]): F1[(SO, Either[E, A])] =
    F.tailRecM(this.provideSet(r, init).widenF[F1]) { c =>
      c.step((), ()) match {
        case now: StepResult.Now[SO, E, A]                => F.pure(Right((now.state, now.result)))
        case wrap: StepResult.Wrap[F1, r, s, SO, E, m, A] => F.map(wrap.provided)(Left(_))
      }
    }

  def stepUnit(init: SI)(implicit ev: Unit <:< R): StepResult[F, SO, E, A] = step((), init)

  def run(r: R, init: SI)(implicit runner: CalcRunner[F]): (SO, Either[E, A]) = runner(this)(r, init)
}

object CalcM {
  def unit[S]: CalcM[Nothing, Any, S, S, Nothing, Unit]                           = Pure(())
  def pure[S, A](a: A): CalcM[Nothing, Any, S, S, Nothing, A]                     = Pure(a)
  def read[S, R]: CalcM[Nothing, R, S, S, Nothing, R]                             = Read()
  def get[S]: CalcM[Nothing, Any, S, S, Nothing, S]                               = Get()
  def set[S](s: S): CalcM[Nothing, Any, Any, S, Nothing, S]                       = Set(s)
  def update[S1, S2](f: S1 => S2): CalcM[Nothing, Any, S1, S2, Nothing, S2]       =
    get[S1].flatMapS(s => set(f(s)))
  def state[S1, S2, A](f: S1 => (S2, A)): CalcM[Nothing, Any, S1, S2, Nothing, A] =
    get[S1].flatMapS { s1 =>
      val (s2, a) = f(s1)
      set(s2) as a
    }

  def raise[S, E](e: E): CalcM[Nothing, Any, S, S, E, Nothing]       = Raise(e)
  def defer[F[+_], R, S1, S2, E, A](x: => CalcM[F, R, S1, S2, E, A]) = Defer(() => x)
  def delay[S, A](x: => A): CalcM[Nothing, Any, S, S, Nothing, A]    = defer(pure[S, A](x))

  def write[S](s: S)(implicit S: Monoid[S]): CalcM[Nothing, Any, S, S, Nothing, S] = update(S.combine(_, s))

  sealed trait CalcMRes[-R, -S1, +S2, +E, +A] extends CalcM[Nothing, R, S1, S2, E, A]   {
    def submit[X](submit: Submit[R, S1, S2, E, A, X]): X
    def mapK[G[+_]](fk: Nothing ~> G): CalcM[G, R, S1, S2, E, A] = this
    override def flatMapK[G[+_], R1 <: R, SI1 <: S1, SO1 >: S2, E1 >: E](
        fk: FunK[Nothing, CalcM[G, R1, SI1, SO1, E1, +*]]
    ): CalcM[G, R, S1, S2, E, A]                                 = this
  }
  final case class Pure[S, +A](a: A)          extends CalcMRes[Any, S, S, Nothing, A]   {
    def submit[X](submit: Submit[Any, S, S, Nothing, A, X]): X                                             = submit.success(submit.state, a)
    def local[R1](f: R1 => Any): CalcM[Nothing, R1, S, S, Nothing, A]                                      = this
    override def dimapState[SI1, SO1](f: SI1 => S, g: S => SO1): CalcM[Nothing, Any, SI1, SO1, Nothing, A] =
      update(g compose f) as_ a

    // def hyperFlatMap[G[+_], R1, SI1, SO1, E1](
    //     fk: Nothing FunK CalcM[G, R1, SI1, SO1, E1, *],
    //     fr: R1 => Any,
    //     fsi: SI1 => S,
    //     fso: S => SO1,
    //     fe: Nothing => E1
    // ): CalcM[G, R1, SI1, SO1, E1, A]                                  = Pure(a)
  }
  final case class Read[S, R]()               extends CalcMRes[R, S, S, Nothing, R]     {
    def submit[X](submit: Submit[R, S, S, Nothing, R, X]): X        = submit.success(submit.state, submit.read)
    def local[R1](f: R1 => R): CalcM[Nothing, R1, S, S, Nothing, R] = Read[S, R1].map(f)
  }
  final case class Get[S]()                   extends CalcMRes[Any, S, S, Nothing, S]   {
    def submit[X](submit: Submit[Any, S, S, Nothing, S, X]): X                                             = submit.success(submit.state, submit.state)
    def local[R1](f: R1 => Any): CalcM[Nothing, R1, S, S, Nothing, S]                                      = this
    override def dimapState[SI1, SO1](f: SI1 => S, g: S => SO1): CalcM[Nothing, Any, SI1, SO1, Nothing, S] =
      get[SI1] *>> state { s1 =>
        val s = f(s1)
        (g(s), s)
      }
  }
  final case class Set[S](s: S)               extends CalcMRes[Any, Any, S, Nothing, S] {
    def submit[X](submit: Submit[Any, Any, S, Nothing, S, X]): X                                             = submit.success(s, s)
    def local[R1](f: R1 => Any): CalcM[Nothing, R1, Any, S, Nothing, S]                                      = this
    override def dimapState[SI1, SO1](f: SI1 => Any, g: S => SO1): CalcM[Nothing, Any, SI1, SO1, Nothing, S] =
      set(g(s)) as_ s
  }
  final case class Raise[S, E](e: E)          extends CalcMRes[Any, S, S, E, Nothing]   {
    def submit[X](submit: Submit[Any, S, S, E, Nothing, X]): X                                             = submit.error(submit.state, e)
    def local[R1](f: R1 => Any): CalcM[Nothing, R1, S, S, E, Nothing]                                      = this
    override def dimapState[SI1, SO1](f: SI1 => S, g: S => SO1): CalcM[Nothing, Any, SI1, SO1, E, Nothing] =
      update(g compose f).swap errorAs_ e
  }
  final case class Defer[+F[+_], -R, -S1, +S2, +E, +A](runStep: () => CalcM[F, R, S1, S2, E, A])
      extends CalcM[F, R, S1, S2, E, A] {
    def mapK[G[+_]](fk: F FunK G): CalcM[G, R, S1, S2, E, A] = Defer(() => runStep().mapK(fk))

    def local[R1](f: R1 => R): CalcM[F, R1, S1, S2, E, A] = Defer(() => runStep().local(f))
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

    def local[R2](f: R2 => Any): CalcM[F, R2, S1, S2, E, A] = this
  }

  abstract class Sub[+F[+_], -S1, +S2, A](val fa: F[A]) extends CalcM[F, Any, S1, S2, Nothing, A] {
    def iss: S1 As S2
    def subcalc[F1[+_], R, SX, E, A1](calc: CalcM[F1, R, S2, SX, E, A1]): CalcM[F1, R, S1, SX, E, A1] =
      iss.substitute[CalcM[F1, R, -*, SX, E, A1]](calc)
    def local[R1](f: R1 => Any): CalcM[F, R1, S1, S2, Nothing, A]                                     = this
  }

  object Sub {

    def apply[F[+_], S, A](fa: F[A]): CalcM[F, Any, S, S, Nothing, A] = new Sub[F, S, S, A](fa) {
      def iss = As.refl

      def mapK[G[+_]](fk: F FunK G): CalcM[G, Any, S, S, Nothing, A] = Sub(fk(fa))
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
        continue.map(_.mapK(fk))
      )

    def local[R1](f: R1 => R): CalcM[F, R1, S1, S3, E2, B] = Bind(
      src.local(f),
      continue.map(_.local(f))
    )
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

  implicit class CalcSimpleStateOps[F[+_], S1, S2, A](private val calc: CalcM[Nothing, Any, S1, S2, Nothing, A])
      extends AnyVal {
    final def runSuccessUnit(init: S1): (S2, A) = calc.runSuccess((), init)

    def toState: IndexedState[S1, S2, A] = IndexedState(runSuccessUnit)
  }

  @tailrec
  def step[F[+_], R, S1, S2, E, A](calc: CalcM[F, R, S1, S2, E, A], r: R, init: S1): StepResult[F, S2, E, A] =
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

            step(p.inner.bind[F, p.R1, E, S2, A](kcont), p.r, init)
          case c2: Bind[F, R, S1, s2, _, e2, _, a2, _]   =>
            step(c2.src.bind(Continue.compose(c2.continue, c1.continue)), r, init)
        }
    }

  implicit def calcFunctorInstance[F[+_], R, S, E]: CalcMonadInstance[F, R, S, E] =
    new CalcMonadInstance[F, R, S, E]

  class CalcMonadInstance[F[+_], R, S, E]
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

  class CalcBindInstance[F[+_], R, S] extends StackSafeBind[CalcM[F, R, S, S, *, *]] {
    override def pure[E, A](a: A): CalcM[F, R, S, S, E, A] = CalcM.pure(a)

    override def raise[E, A](e: E): CalcM[F, R, S, S, E, A] = CalcM.raise(e)

    override def foldWith[E, A, X, B](
        fa: CalcM[F, R, S, S, E, A],
        h: E => CalcM[F, R, S, S, X, B],
        f: A => CalcM[F, R, S, S, X, B]
    ): CalcM[F, R, S, S, X, B] = fa.foldWith(f, h)
  }

  class CalcContextInstance[F[+_], R, S, E] extends WithRun[CalcM[F, R, S, S, E, *], CalcM[F, Any, S, S, E, *], R] {
    override val context: CalcM[F, R, S, S, E, R]                    = CalcM.read
    override val functor: Functor[tofu.data.CalcM[F, R, S, S, E, *]] = new CalcMonadInstance

    override def runContext[A](fa: CalcM[F, R, S, S, E, A])(ctx: R): CalcM[F, Any, S, S, E, A]   = fa.provide(ctx)
    override def local[A](fa: CalcM[F, R, S, S, E, A])(project: R => R): CalcM[F, R, S, S, E, A] =
      fa.provideSome(project)
    override def lift[A](fa: CalcM[F, Any, S, S, E, A]): CalcM[F, R, S, S, E, A]                 = fa
  }
}

object CalcMSpecials {
  trait Continue[-A, -E, +C] { self =>
    def success(a: A): C
    def error(e: E): C

    def map[D](f: C => D): Continue[A, E, D] = new Continue[A, E, D] {
      override def success(a: A): D = f(self.success(a))
      override def error(e: E): D   = f(self.error(e))
    }
  }

  object Continue {
    def apply[A, E, X](f: A => X, h: E => X): Continue[A, E, X] = new Continue[A, E, X] {
      override def success(a: A): X = f(a)
      override def error(e: E): X   = h(e)
    }

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

    def swap[A, E, S, X >: CalcM[Nothing, Any, S, S, A, E]]: Continue[A, E, X] =
      new Continue[A, E, X] {
        override def success(a: A): X = CalcM.Raise(a)
        override def error(e: E): X   = CalcM.Pure(e)
      }

    def update[A, E, SI, SO, X >: CalcM[Nothing, Any, SI, SO, E, A]](f: SI => SO): Continue[A, E, X] =
      new Continue[A, E, X] {
        override def success(a: A): X = CalcM.update(f) as_ a
        override def error(e: E): X   = CalcM.update(f).swap errorAs_ e
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

trait CalcRunner[-F[+_]] {
  def apply[R, SI, SO, E, A](calc: CalcM[F, R, SI, SO, E, A])(r: R, init: SI): (SO, Either[E, A])
}

object CalcRunner {
  implicit val nothingRunner: CalcRunner[Nothing] =
    new CalcRunner[Nothing] {
      def apply[R, SI, SO, E, A](calc: CalcM[Nothing, R, SI, SO, E, A])(r: R, init: SI): (SO, Either[E, A]) =
        calc.step(r, init) match {
          case wrap: StepResult.Wrap[Nothing, _, _, SO, E, m, A] => wrap.inner: Nothing
          case StepResult.Error(s, err)                          => (s, Left(err))
          case StepResult.Ok(s, a)                               => (s, Right(a))
        }
    }
}
