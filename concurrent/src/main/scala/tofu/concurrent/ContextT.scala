package tofu.concurrent
import cats._
import cats.effect._
import cats.tagless.InvariantK
import tofu.concurrent.impl._
import tofu.lift.{Rebase, Unlift}
import tofu.optics.Contains
import tofu.syntax.funk.{funK, funKFrom}
import tofu.syntax.monadic._
import tofu.{HasContextRun, WithLocal}

/** a ReaderT analog, allowing to have context referring resulting type
  *  for instance you can define
  * {{{
  *  case class MyCtx[F[_]](state: Ref[F, Int])
  *
  *  type MyProcess[+A] = ContextT[IO, MyCtx, A]
  * }}}
  *
  * here MyProcess is equivalent to a function alias
  * {{{type MyProcess[+A] => Ref[MyProcess, Int] => IO[A]}}}
  * which would be problematic to define using normal `ReaderT`, `Env` or `ZIO` type constructors
  */
trait ContextT[F[+_], C[_[_]], +A] { self =>

  /** run computation, providing context */
  def run(c: C[ContextT[F, C, +*]]): F[A]

  final def imapK[G[+_]](f: F ~> G)(g: G ~> F)(implicit cf: InvariantK[C]): ContextT[G, C, A] = {
    lazy val fc: ContextT[F, C, *] ~> ContextT[G, C, *] = funK(_.imapKInt(f, fc, gc))
    lazy val gc: ContextT[G, C, *] ~> ContextT[F, C, *] = funK(_.imapKInt(g, gc, fc))
    imapKInt(f, fc, gc)
  }
  final private def imapKInt[G[+_]](
      f: F ~> G,
      fc: => ContextT[F, C, *] ~> ContextT[G, C, *],
      gc: => ContextT[G, C, *] ~> ContextT[F, C, *]
  )(implicit ci: InvariantK[C]): ContextT[G, C, A] =
    cg => f(run(ci.imapK(cg)(gc)(fc)))
}

object ContextT extends ContextTInstances {
  private def makeLiftF[F[+_], C[_[_]]]: F ~> ContextT[F, C, *] = funKFrom[F](fa => _ => fa)
  private val liftFAny                                          = makeLiftF[Any, Any]

  /** lift pure value to reader */
  def pure[F[+_]: Applicative, C[_[_]], A](a: A): ContextT[F, C, A] = _ => a.pure[F]

  /** acquire context */
  def ask[F[+_]: Applicative, C[_[_]]]: ContextT[F, C, C[ContextT[F, C, *]]] = _.pure[F]

  /** get some value from context */
  def provide[F[+_]: Applicative, C[_[_]], A](f: C[ContextT[F, C, *]] => A): ContextT[F, C, A] = c => f(c).pure[F]

  /** construct some process from context */
  def provideF[F[+_], C[_[_]], A](f: C[ContextT[F, C, *]] => F[A]): ContextT[F, C, A] = c => f(c)

  /** construct some process, using resulting type */
  def provideM[F[+_], C[_[_]], A](f: C[ContextT[F, C, *]] => ContextT[F, C, A]): ContextT[F, C, A] = c => f(c).run(c)

  /** Natural transformation for lifting underlying values to contextual */
  def liftF[F[+_], C[_[_]]]: F ~> ContextT[F, C, *] = liftFAny.asInstanceOf[F ~> ContextT[F, C, *]]

  /** lift underlying value to contextual */
  def lift[F[+_], C[_[_]], A](fa: F[A]): ContextT[F, C, A] = _ => fa

  def transfer[In[_[_]], Out[_[_]], F[+_]: Monad](c1: In[ContextT[F, In, *]])(implicit
      l: Out[ContextT[F, Out, *]] Contains In[ContextT[F, Out, *]],
      rc1: Rebase[In],
  ): In[ContextT[F, Out, *]] = rc1.rebase(c1)

}

trait ContextTInstances extends ContextTInstancesQ

trait ContextTInstancesZ { self: ContextTInstances =>
  final implicit def contextTInvariant[F[+_]: Invariant, C[_[_]]]: Invariant[ContextT[F, C, *]] = new ContextTInvariantI
}

trait ContextTInstancesY extends ContextTInstancesZ { self: ContextTInstances =>
  final implicit def contextTFunctor[F[+_]: Functor, C[_[_]]]: Functor[ContextT[F, C, *]] = new ContextTFunctorI

  final implicit def contextTInvariantSemigroupal[F[+_]: InvariantSemigroupal, C[_[_]]]
      : InvariantSemigroupal[ContextT[F, C, *]] = new ContextTInvariantSemigroupalI

  final implicit def contextTSemigroupK[F[+_]: SemigroupK, C[_[_]]]: SemigroupK[ContextT[F, C, *]] =
    new ContextTSemigroupKI
}

trait ContextTInstancesX extends ContextTInstancesY { self: ContextTInstances =>
  final implicit def contextTApply[F[+_]: Apply, C[_[_]]]: Apply[ContextT[F, C, *]]             = new ContextTApplyI
  final implicit def contextTMonoidK[F[+_]: MonoidK, C[_[_]]]: MonoidK[ContextT[F, C, *]]       = new ContextTMonoidKI
  final implicit def contextTcoflatMap[F[+_]: CoflatMap, C[_[_]]]: CoflatMap[ContextT[F, C, *]] = new ContextTCoflatMapI

  final implicit def contextTInvariantMonoidal[F[+_]: InvariantMonoidal, C[_[_]]]
      : InvariantMonoidal[ContextT[F, C, *]] = new ContextTInvariantMonoidalI
}

trait ContextTInstancesW extends ContextTInstancesX { self: ContextTInstances =>
  final implicit def contextTApplicative[F[+_]: Applicative, C[_[_]]]: Applicative[ContextT[F, C, *]] =
    new ContextTApplicativeI

  final implicit def contextTFlatMap[F[+_]: FlatMap, C[_[_]]]: FlatMap[ContextT[F, C, *]] = new ContextTFlatMapI
}

trait ContextTInstancesV extends ContextTInstancesW { self: ContextTInstances =>
  final implicit def contextTMonad[F[+_]: Monad, C[_[_]]]: Monad[ContextT[F, C, *]] = new ContextTMonadI

  final implicit def contextTAlternative[F[+_]: Alternative, C[_[_]]]: Alternative[ContextT[F, C, *]] =
    new ContextTAlternativeI

  final implicit def contextTApplicativeError[F[+_]: ApplicativeError[*[_], E], C[_[_]], E]
      : ApplicativeError[ContextT[F, C, *], E] =
    new ContextTApplicativeErrorI
}

trait ContextTInstancesU extends ContextTInstancesV { self: ContextTInstances =>
  final implicit def contextTMonadError[F[+_]: MonadError[*[_], E], C[_[_]], E]: MonadError[ContextT[F, C, *], E] =
    new ContextTMonadErrorI
}

trait ContextTInstancesT extends ContextTInstancesU { self: ContextTInstances =>
  final implicit def contextTBracket[F[+_]: Bracket[*[_], E], C[_[_]], E]: Bracket[ContextT[F, C, *], E] =
    new ContextTBracketI
}

trait ContextTInstancesS extends ContextTInstancesT { self: ContextTInstances =>

  final implicit def contextTSync[F[+_]: Sync, C[_[_]]]: Sync[ContextT[F, C, *]]       = new ContextTSyncI
  final implicit def contextTLiftIO[F[+_]: LiftIO, C[_[_]]]: LiftIO[ContextT[F, C, *]] = new ContextTLiftIOI

  final implicit def contextTSubContext[F[+_]: Applicative: Defer, C[_[_]], X](implicit
      lens: C[ContextT[F, C, *]] Contains X
  ): WithLocal[ContextT[F, C, *], X] = contextTRunContext[F, C].subcontext(lens)
}

trait ContextTInstancesR extends ContextTInstancesS { self: ContextTInstances =>
  final implicit def contextTAsync[F[+_]: Async, C[_[_]]]: Async[ContextT[F, C, *]] = new ContextTAsyncI

  final implicit def contextTContext[F[+_]: Applicative, C[_[_]]]: ContextTContext[F, C] =
    new ContextTContext

  final implicit def contextTNonEmptyParallel[F[+_]: NonEmptyParallel, C[_[_]]: InvariantK] =
    new ContextTNonEmptyParallelI[F, C]
}

trait ContextTInstancesQ extends ContextTInstancesR { self: ContextTInstances =>
  final implicit def contextTConcurrent[F[+_]: Concurrent, C[_[_]]]: Concurrent[ContextT[F, C, *]] =
    new ContextTConcurrentI

  final implicit def contextTRunContext[F[+_]: Applicative: Defer, C[_[_]]]: ContextTRunContext[F, C] =
    new ContextTRunContext

  final implicit def runContextUnsafe[F[+_]: Applicative, C[_[_]]]
      : HasContextRun[ContextT[F, C, *], F, C[ContextT[F, C, *]]] =
    new ContextTRunContextUnsafe[F, C]

  final implicit def contextTParallel[F[+_]: Parallel, C[_[_]]: InvariantK]: Parallel[ContextT[F, C, *]] =
    new ContextTParallelI[F, C]

  final implicit def contextTTimer[F[+_], C[_[_]]](implicit t: Timer[F]): Timer[ContextT[F, C, *]] =
    Timer.TimerOps[F](t).mapK(ContextT.liftF)

  final implicit def contextTContextShift[F[+_], C[_[_]]](implicit
      cs: ContextShift[F]
  ): ContextShift[ContextT[F, C, *]] =
    new ContextTContextShift[F, C]

  final implicit def contextTUnlifting[F[+_]: Monad, In[_[_]], Out[_[_]]](implicit
      l: Out[ContextT[F, Out, *]] Contains In[ContextT[F, Out, *]],
      rc1: Rebase[In],
  ): Unlift[ContextT[F, In, *], ContextT[F, Out, *]] = {
    class uloi(implicit c2c: Out[ContextT[F, Out, *]]) extends Unlift[ContextT[F, Out, *], ContextT[F, In, *]] {
      implicit def self                                         = this
      def lift[A](c2a: ContextT[F, Out, A]): ContextT[F, In, A] =
        c1c => c2a.run(l.set(c2c, rc1.rebase(c1c)))

      def in2out[A](c1a: ContextT[F, In, A]): ContextT[F, Out, A] =
        c2c => c1a.run(rc1.rebase(l.get(c2c)))

      def unlift: ContextT[F, In, ContextT[F, In, *] ~> ContextT[F, Out, *]] =
        _ => funKFrom[ContextT[F, In, *]](in2out(_)).pure[F]
    }

    implicit def uloi(implicit c2c: Out[ContextT[F, Out, *]]): Unlift[ContextT[F, Out, *], ContextT[F, In, *]] =
      new uloi

    implicit object ulio extends Unlift[ContextT[F, In, *], ContextT[F, Out, *]] {
      def lift[A](c1a: ContextT[F, In, A]): ContextT[F, Out, A] =
        implicit c2c => c1a.run(rc1.rebase(l.get(c2c)))

      def t21[A](c2c: Out[ContextT[F, Out, *]])(c2a: ContextT[F, Out, A]): ContextT[F, In, A] =
        c1c => c2a.run(l.set(c2c, rc1.rebase(c1c)(implicitly, this)))

      def unlift: ContextT[F, Out, ContextT[F, Out, *] ~> ContextT[F, In, *]] =
        c2c => funKFrom[ContextT[F, Out, *]](t21(c2c) _).pure[F]
    }

    ulio
  }
}
