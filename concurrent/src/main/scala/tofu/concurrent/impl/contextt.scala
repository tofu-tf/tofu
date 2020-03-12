package tofu.concurrent
package impl
import cats.effect._
import cats.instances.either._
import cats.instances.tuple._
import cats.syntax.bifunctor._
import cats._
import tofu.{Context, HasContextRun, RunContext, WithContext, WithRun}
import tofu.syntax.funk.funKFrom
import tofu.syntax.monadic._

trait ContextTInvariant[F[+_], C[_[_]]] extends Invariant[ContextT[F, C, *]] {
  implicit def F: Invariant[F]
  final override def imap[A, B](fa: ContextT[F, C, A])(f: A => B)(g: B => A): ContextT[F, C, B] = c =>
    F.imap(fa.run(c))(f)(g)
}

final class ContextTInvariantI[F[+_], C[_[_]]](implicit val F: Invariant[F]) extends ContextTInvariant[F, C]

trait ContextTFunctor[F[+_], C[_[_]]] extends Functor[ContextT[F, C, *]] with ContextTInvariant[F, C] {
  implicit def F: Functor[F]

  final override def map[A, B](fa: ContextT[F, C, A])(f: A => B): ContextT[F, C, B] = fa.run(_).map(f)
  final override def void[A](fa: ContextT[F, C, A]): ContextT[F, C, Unit]           = fa.run(_).void

  final override def widen[A, B >: A](fa: ContextT[F, C, A]): ContextT[F, C, B] = fa

  final override def as[A, B](fa: ContextT[F, C, A], b: B): ContextT[F, C, B]             = c => fa.run(c).as(b)
  final override def tupleLeft[A, B](fa: ContextT[F, C, A], b: B): ContextT[F, C, (B, A)] = fa.run(_).tupleLeft(b)

  override def fproduct[A, B](fa: ContextT[F, C, A])(f: A => B): ContextT[F, C, (A, B)] = fa.run(_).fproduct(f)
  override def tupleRight[A, B](fa: ContextT[F, C, A], b: B): ContextT[F, C, (A, B)]    = fa.run(_).tupleRight(b)
  override def ifF[A](fb: ContextT[F, C, Boolean])(ifTrue: => A, ifFalse: => A): ContextT[F, C, A] = c =>
    F.ifF(fb.run(c))(ifTrue, ifFalse)
}

trait ContextTInvariantSemigroupal[F[+_], C[_[_]]]
    extends InvariantSemigroupal[ContextT[F, C, *]] with ContextTInvariant[F, C] {
  implicit def F: InvariantSemigroupal[F]

  final override def product[A, B](fa: ContextT[F, C, A], fb: ContextT[F, C, B]): ContextT[F, C, (A, B)] =
    c => F.product(fa.run(c), fb.run(c))
}

final class ContextTInvariantSemigroupalI[F[+_], C[_[_]]](implicit val F: InvariantSemigroupal[F])
    extends ContextTInvariantSemigroupal[F, C]

trait ContextTInvariantMonoidal[F[+_], C[_[_]]]
    extends InvariantMonoidal[ContextT[F, C, *]] with ContextTInvariantSemigroupal[F, C] {
  implicit def F: InvariantMonoidal[F]

  final override val unit: ContextT[F, C, Unit]        = _ => F.unit
  final override def point[A](a: A): ContextT[F, C, A] = _ => F.point(a)
}

final class ContextTInvariantMonoidalI[F[+_], C[_[_]]](implicit val F: InvariantMonoidal[F])
    extends ContextTInvariantMonoidal[F, C]

final class ContextTFunctorI[F[+_], C[_[_]]](implicit val F: Functor[F]) extends ContextTFunctor[F, C]

trait ContextTApply[F[+_], C[+_[_]]]
    extends Apply[ContextT[F, C, *]] with ContextTFunctor[F, C] with ContextTInvariantSemigroupal[F, C] {
  implicit def F: Apply[F]
  final override def ap[A, B](ff: ContextT[F, C, A => B])(fa: ContextT[F, C, A]): ContextT[F, C, B] =
    c => ff.run(c) ap fa.run(c)

  final override def map2[A, B, Z](fa: ContextT[F, C, A], fb: ContextT[F, C, B])(f: (A, B) => Z): ContextT[F, C, Z] =
    c => fa.run(c).map2(fb.run(c))(f)

  final override def productR[A, B](fa: ContextT[F, C, A])(fb: ContextT[F, C, B]): ContextT[F, C, B] =
    c => fa.run(c).productR(fb.run(c))
  final override def productL[A, B](fa: ContextT[F, C, A])(fb: ContextT[F, C, B]): ContextT[F, C, A] =
    c => fa.run(c).productL(fb.run(c))

  final override def ap2[A, B, Z](
      ff: ContextT[F, C, (A, B) => Z]
  )(fa: ContextT[F, C, A], fb: ContextT[F, C, B]): ContextT[F, C, Z] =
    c => ff.run(c).ap2(fa.run(c), fb.run(c))
  final override def map2Eval[A, B, Z](fa: ContextT[F, C, A], fb: Eval[ContextT[F, C, B]])(
      f: (A, B) => Z
  ): Eval[ContextT[F, C, Z]] =
    Eval.always(c => fa.run(c).map2Eval(fb.map(_.run(c)))(f).value)
  final override def ifA[A](
      fcond: ContextT[F, C, Boolean]
  )(ifTrue: ContextT[F, C, A], ifFalse: ContextT[F, C, A]): ContextT[F, C, A] =
    c => F.ifA(fcond.run(c))(ifTrue.run(c), ifFalse.run(c))
}

final class ContextTApplyI[F[+_], C[_[_]]](implicit val F: Apply[F]) extends ContextTApply[F, C]

trait ContextTApplicative[F[+_], C[_[_]]]
    extends Applicative[ContextT[F, C, *]] with ContextTApply[F, C] with ContextTInvariantMonoidal[F, C] {
  implicit def F: Applicative[F]
  final override def pure[A](x: A): ContextT[F, C, A] = _ => x.pure[F]

  final override def replicateA[A](n: Int, fa: ContextT[F, C, A]): ContextT[F, C, List[A]] = fa.run(_).replicateA(n)
  final override def unlessA[A](cond: Boolean)(f: => ContextT[F, C, A]): ContextT[F, C, Unit] = c =>
    F.unlessA(cond)(f.run(c))
  final override def whenA[A](cond: Boolean)(f: => ContextT[F, C, A]): ContextT[F, C, Unit] = c =>
    F.whenA(cond)(f.run(c))
}

final class ContextTApplicativeI[F[+_], C[_[_]]](implicit val F: Applicative[F]) extends ContextTApplicative[F, C]

trait ContextTFlatMap[F[+_], C[_[_]]] extends FlatMap[ContextT[F, C, *]] with ContextTApply[F, C] {
  implicit def F: FlatMap[F]

  final override def flatMap[A, B](fa: ContextT[F, C, A])(f: A => ContextT[F, C, B]): ContextT[F, C, B] =
    c => fa.run(c).flatMap(f(_).run(c))

  final override def tailRecM[A, B](a: A)(f: A => ContextT[F, C, Either[A, B]]): ContextT[F, C, B] =
    c => a.tailRecM(a => f(a).run(c))

  final override def flatten[A](ffa: ContextT[F, C, ContextT[F, C, A]]): ContextT[F, C, A] =
    c => ffa.run(c).flatMap(_.run(c))

  final override def productREval[A, B](fa: ContextT[F, C, A])(fb: Eval[ContextT[F, C, B]]): ContextT[F, C, B] =
    c => F.productREval(fa.run(c))(fb.map(_.run(c)))
  final override def productLEval[A, B](fa: ContextT[F, C, A])(fb: Eval[ContextT[F, C, B]]): ContextT[F, C, A] =
    c => F.productLEval(fa.run(c))(fb.map(_.run(c)))
  final override def mproduct[A, B](fa: ContextT[F, C, A])(f: A => ContextT[F, C, B]): ContextT[F, C, (A, B)] =
    c => F.mproduct(fa.run(c))(a => f(a).run(c))
  final override def ifM[B](
      fa: ContextT[F, C, Boolean]
  )(ifTrue: => ContextT[F, C, B], ifFalse: => ContextT[F, C, B]): ContextT[F, C, B] =
    c => F.ifM(fa.run(c))(ifTrue.run(c), ifFalse.run(c))
  final override def flatTap[A, B](fa: ContextT[F, C, A])(f: A => ContextT[F, C, B]): ContextT[F, C, A] =
    c => F.flatTap(fa.run(c))(a => f(a).run(c))
  final override def foreverM[A, B](fa: ContextT[F, C, A]): ContextT[F, C, B] =
    c => F.foreverM(fa.run(c))
  final override def iterateForeverM[A, B](a: A)(f: A => ContextT[F, C, A]): ContextT[F, C, B] =
    c => F.iterateForeverM(a)(a => f(a).run(c))
  final override def untilDefinedM[A](foa: ContextT[F, C, Option[A]]): ContextT[F, C, A] =
    c => F.untilDefinedM(foa.run(c))
}

final class ContextTFlatMapI[F[+_], C[_[_]]](implicit val F: FlatMap[F]) extends ContextTFlatMap[F, C]

trait ContextTMonad[F[+_], C[_[_]]]
    extends Monad[ContextT[F, C, *]] with ContextTApplicative[F, C] with ContextTFlatMap[F, C] {
  implicit def F: Monad[F]

  final override def whileM[G[_], A](
      p: ContextT[F, C, Boolean]
  )(body: => ContextT[F, C, A])(implicit G: Alternative[G]): ContextT[F, C, G[A]] =
    c => F.whileM(p.run(c))(body.run(c))
  final override def whileM_[A](p: ContextT[F, C, Boolean])(body: => ContextT[F, C, A]): ContextT[F, C, Unit] =
    c => F.whileM_(p.run(c))(body.run(c))
  final override def untilM[G[_], A](
      f: ContextT[F, C, A]
  )(cond: => ContextT[F, C, Boolean])(implicit G: Alternative[G]): ContextT[F, C, G[A]] =
    c => F.untilM(f.run(c))(cond.run(c))
  final override def untilM_[A](f: ContextT[F, C, A])(cond: => ContextT[F, C, Boolean]): ContextT[F, C, Unit] =
    c => F.untilM_(f.run(c))(cond.run(c))
  final override def iterateWhile[A](f: ContextT[F, C, A])(p: A => Boolean): ContextT[F, C, A] =
    f.run(_).iterateWhile(p)
  final override def iterateUntil[A](f: ContextT[F, C, A])(p: A => Boolean): ContextT[F, C, A] =
    f.run(_).iterateUntil(p)
  final override def iterateWhileM[A](init: A)(f: A => ContextT[F, C, A])(p: A => Boolean): ContextT[F, C, A] =
    c => F.iterateWhileM(init)(f(_).run(c))(p)
  final override def iterateUntilM[A](init: A)(f: A => ContextT[F, C, A])(p: A => Boolean): ContextT[F, C, A] =
    c => F.iterateUntilM(init)(f(_).run(c))(p)
}

final class ContextTMonadI[F[+_], C[_[_]]](implicit val F: Monad[F]) extends ContextTMonad[F, C]

trait ContextTApplicativeError[F[+_], C[_[_]], E]
    extends ApplicativeError[ContextT[F, C, *], E] with ContextTApplicative[F, C] {
  implicit def F: ApplicativeError[F, E]
  final override def raiseError[A](e: E): ContextT[F, C, A] = _ => F.raiseError(e)
  final override def handleErrorWith[A](fa: ContextT[F, C, A])(f: E => ContextT[F, C, A]): ContextT[F, C, A] =
    c => F.handleErrorWith(fa.run(c))(e => f(e).run(c))

}

trait ContextTSemigroupK[F[+_], C[_[_]]] extends SemigroupK[ContextT[F, C, *]] {
  implicit def F: SemigroupK[F]

  final override def combineK[A](x: ContextT[F, C, A], y: ContextT[F, C, A]): ContextT[F, C, A] = c =>
    F.combineK(x.run(c), y.run(c))
}

final class ContextTSemigroupKI[F[+_], C[_[_]]](implicit val F: SemigroupK[F]) extends ContextTSemigroupK[F, C]

trait ContextTMonoidK[F[+_], C[_[_]]] extends MonoidK[ContextT[F, C, *]] with ContextTSemigroupK[F, C] {
  implicit def F: MonoidK[F]
  private[this] val emptyAny: ContextT[F, C, Any] = _ => F.empty[Any]

  final override def empty[A]: ContextT[F, C, A] = emptyAny.asInstanceOf[ContextT[F, C, A]]
}

final class ContextTMonoidKI[F[+_], C[_[_]]](implicit val F: MonoidK[F]) extends ContextTMonoidK[F, C]

trait ContextTAlternative[F[+_], C[_[_]]]
    extends Alternative[ContextT[F, C, *]] with ContextTMonoidK[F, C] with ContextTApplicative[F, C] {
  implicit def F: Alternative[F]
}

final class ContextTAlternativeI[F[+_], C[_[_]]](implicit val F: Alternative[F]) extends ContextTAlternative[F, C]

trait ContextTCoflatMap[F[+_], C[_[_]]] extends CoflatMap[ContextT[F, C, *]] with ContextTFunctor[F, C] {
  implicit def F: CoflatMap[F]

  def coflatMap[A, B](fa: ContextT[F, C, A])(f: ContextT[F, C, A] => B): ContextT[F, C, B] =
    c => F.coflatMap(fa.run(c))(fa => f(_ => fa))
}

final class ContextTCoflatMapI[F[+_], C[_[_]]](implicit val F: CoflatMap[F]) extends ContextTCoflatMap[F, C]

final class ContextTApplicativeErrorI[F[+_], C[_[_]], E](implicit val F: ApplicativeError[F, E])
    extends ContextTApplicativeError[F, C, E]

trait ContextTMonadError[F[+_], C[_[_]], E]
    extends MonadError[ContextT[F, C, *], E] with ContextTMonad[F, C] with ContextTApplicativeError[F, C, E] {
  implicit def F: MonadError[F, E]
  final override def ensure[A](fa: ContextT[F, C, A])(error: => E)(predicate: A => Boolean): ContextT[F, C, A] =
    c => F.ensure(fa.run(c))(error)(predicate)
  final override def ensureOr[A](fa: ContextT[F, C, A])(error: A => E)(predicate: A => Boolean): ContextT[F, C, A] =
    c => F.ensureOr(fa.run(c))(error)(predicate)
  final override def rethrow[A, EE <: E](fa: ContextT[F, C, Either[EE, A]]): ContextT[F, C, A] =
    c => F.rethrow(fa.run(c))
  final override def redeemWith[A, B](
      fa: ContextT[F, C, A]
  )(recover: E => ContextT[F, C, B], bind: A => ContextT[F, C, B]): ContextT[F, C, B] =
    c => F.redeemWith(fa.run(c))(e => recover(e).run(c), a => bind(a).run(c))
  final override def adaptError[A](fa: ContextT[F, C, A])(pf: PartialFunction[E, E]): ContextT[F, C, A] =
    c => F.adaptError(fa.run(c))(pf)
}

final class ContextTMonadErrorI[F[+_], C[_[_]], E](implicit val F: MonadError[F, E]) extends ContextTMonadError[F, C, E]

trait ContextTBracket[F[+_], C[_[_]], E] extends Bracket[ContextT[F, C, *], E] with ContextTMonadError[F, C, E] {
  implicit def F: Bracket[F, E]

  final override def bracketCase[A, B](
      acquire: ContextT[F, C, A]
  )(use: A => ContextT[F, C, B])(release: (A, ExitCase[E]) => ContextT[F, C, Unit]): ContextT[F, C, B] =
    c => F.bracketCase(acquire.run(c))(a => use(a).run(c))((a, ec) => release(a, ec).run(c))
  final override def bracket[A, B](
      acquire: ContextT[F, C, A]
  )(use: A => ContextT[F, C, B])(release: A => ContextT[F, C, Unit]): ContextT[F, C, B] =
    c => F.bracket(acquire.run(c))(a => use(a).run(c))(a => release(a).run(c))
  final override def uncancelable[A](fa: ContextT[F, C, A]): ContextT[F, C, A] =
    c => F.uncancelable(fa.run(c))
  final override def guarantee[A](fa: ContextT[F, C, A])(finalizer: ContextT[F, C, Unit]): ContextT[F, C, A] =
    c => F.guarantee(fa.run(c))(finalizer.run(c))
  final override def guaranteeCase[A](
      fa: ContextT[F, C, A]
  )(finalizer: ExitCase[E] => ContextT[F, C, Unit]): ContextT[F, C, A] =
    c => F.guaranteeCase(fa.run(c))(ec => finalizer(ec).run(c))
  final override def onCancel[A](fa: ContextT[F, C, A])(finalizer: ContextT[F, C, Unit]): ContextT[F, C, A] =
    c => F.onCancel(fa.run(c))(finalizer.run(c))
}

final class ContextTBracketI[F[+_], C[_[_]], E](implicit val F: Bracket[F, E]) extends ContextTBracket[F, C, E]

trait ContextTSync[F[+_], C[_[_]]] extends Sync[ContextT[F, C, *]] with ContextTBracket[F, C, Throwable] {
  implicit def F: Sync[F]

  final override def suspend[A](thunk: => ContextT[F, C, A]): ContextT[F, C, A] = c => F.suspend(thunk.run(c))
  final override def delay[A](thunk: => A): ContextT[F, C, A]                   = _ => F.delay(thunk)
}

final class ContextTSyncI[F[+_], C[_[_]]](implicit val F: Sync[F]) extends ContextTSync[F, C]

trait ContextTLiftIO[F[+_], C[_[_]]] extends LiftIO[ContextT[F, C, *]] {
  implicit def F: LiftIO[F]
  final override def liftIO[A](ioa: IO[A]): ContextT[F, C, A] = _ => F.liftIO(ioa)

}

final class ContextTLiftIOI[F[+_], C[_[_]]](implicit val F: LiftIO[F]) extends ContextTLiftIO[F, C]

trait ContextTAsync[F[+_], C[_[_]]] extends Async[ContextT[F, C, *]] with ContextTLiftIO[F, C] with ContextTSync[F, C] {
  implicit def F: Async[F]
  final override def async[A](k: (Either[Throwable, A] => Unit) => Unit): ContextT[F, C, A] = _ => F.async(k)
  final override def asyncF[A](k: (Either[Throwable, A] => Unit) => ContextT[F, C, Unit]): ContextT[F, C, A] =
    c => F.asyncF(cb => k(cb).run(c))
  final override def never[A]: ContextT[F, C, A] = _ => F.never
}
final class ContextTAsyncI[F[+_], C[_[_]]](implicit val F: Async[F]) extends ContextTAsync[F, C]

trait ContextTConcurrent[F[+_], C[_[_]]] extends Concurrent[ContextT[F, C, *]] with ContextTAsync[F, C] {
  implicit def F: Concurrent[F]
  final override def start[A](fa: ContextT[F, C, A]): ContextT[F, C, Fiber[ContextT[F, C, *], A]] =
    c => F.start(fa.run(c)).map(_.mapK(ContextT.liftF))
  final override def racePair[A, B](
      fa: ContextT[F, C, A],
      fb: ContextT[F, C, B]
  ): ContextT[F, C, Either[(A, Fiber[ContextT[F, C, *], B]), (Fiber[ContextT[F, C, *], A], B)]] =
    c => F.racePair(fa.run(c), fb.run(c)).map(_.bimap(_.map(_.mapK(ContextT.liftF)), _.leftMap(_.mapK(ContextT.liftF))))

  final override def race[A, B](fa: ContextT[F, C, A], fb: ContextT[F, C, B]): ContextT[F, C, Either[A, B]] =
    c => F.race(fa.run(c), fb.run(c))
  final override def cancelable[A](
      k: (Either[Throwable, A] => Unit) => CancelToken[ContextT[F, C, *]]
  ): ContextT[F, C, A] =
    c => F.cancelable(cb => k(cb).run(c))
  final override def continual[A, B](
      fa: ContextT[F, C, A]
  )(f: Either[Throwable, A] => ContextT[F, C, B]): ContextT[F, C, B] =
    c => F.continual(fa.run(c))(e => f(e).run(c))
}

final class ContextTConcurrentI[F[+_], C[_[_]]](implicit val F: Concurrent[F]) extends ContextTConcurrent[F, C]

class ContextTContext[F[+_]: Applicative, C[_[_]]] extends WithContext[ContextT[F, C, *], C[ContextT[F, C, *]]] {
  val functor: Functor[ContextT[F, C, *]] = new ContextTFunctorI

  final def context: ContextT[F, C, C[ContextT[F, C, *]]] = _.pure[F]
}

final class ContextTRunContext[F[+_]: Applicative, C[_[_]]](implicit FD: Defer[F])
    extends ContextTContext[F, C] with WithRun[ContextT[F, C, *], F, C[ContextT[F, C, *]]] {
  def runContext[A](fa: ContextT[F, C, A])(ctx: C[ContextT[F, C, *]]): F[A] = fa.run(ctx)
  def local[A](fa: ContextT[F, C, A])(project: C[ContextT[F, C, *]] => C[ContextT[F, C, *]]): ContextT[F, C, A] =
    c => FD.defer(fa.run(project(c)))
}

// instance that does not defer locals. could be stack-unsafe
final class ContextTRunContextUnsafe[F[+_]: Applicative, C[_[_]]]
    extends ContextTContext[F, C] with RunContext[ContextT[F, C, *]] {
  type Lower[+A] = F[A]

  def runContext[A](fa: ContextT[F, C, A])(ctx: C[ContextT[F, C, *]]): F[A] = fa.run(ctx)
  def local[A](fa: ContextT[F, C, A])(project: C[ContextT[F, C, *]] => C[ContextT[F, C, *]]): ContextT[F, C, A] =
    c => fa.run(project(c))
}
