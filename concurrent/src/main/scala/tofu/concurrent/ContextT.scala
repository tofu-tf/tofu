package tofu.concurrent
import cats._
import cats.effect._
import cats.instances.either._
import tofu.{HasContextRun, WithContext, WithRun}
import tofu.concurrent.impl._
import tofu.syntax.funk.funKFrom
import tofu.syntax.monadic._

/** a ReaderT analog, allowing to have context referring resulting type
  *  for instance you can define ```MyCtx[F[_]](state: Ref[F, Int], k ```
  *
  * */
trait ContextT[F[+_], C[_[_]], +A] {

  /** run computation, providing context */
  def run(c: C[ContextT[F, C, +*]]): F[A]
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
}
trait ContextTInstances extends ContextTInstancesQ

trait ContextTInstancesZ {
  final implicit def contextTInvariant[F[+_]: Invariant, C[_[_]]]: Invariant[ContextT[F, C, *]] = new ContextTInvariantI
}

trait ContextTInstancesY {
  final implicit def contextTFunctor[F[+_]: Functor, C[_[_]]]: Functor[ContextT[F, C, *]] = new ContextTFunctorI

  final implicit def contextTInvariantSemigroupal[F[+_]: InvariantSemigroupal, C[_[_]]]
      : InvariantSemigroupal[ContextT[F, C, *]] = new ContextTInvariantSemigroupalI

  final implicit def contextTSemigroupK[F[+_]: SemigroupK, C[_[_]]]: SemigroupK[ContextT[F, C, *]] =
    new ContextTSemigroupKI
}

trait ContextTInstancesX extends ContextTInstancesY {
  final implicit def contextTApply[F[+_]: Apply, C[_[_]]]: Apply[ContextT[F, C, *]]             = new ContextTApplyI
  final implicit def contextTMonoidK[F[+_]: MonoidK, C[_[_]]]: MonoidK[ContextT[F, C, *]]       = new ContextTMonoidKI
  final implicit def contextTcoflatMap[F[+_]: CoflatMap, C[_[_]]]: CoflatMap[ContextT[F, C, *]] = new ContextTCoflatMapI

  final implicit def contextTInvariantMonoidal[F[+_]: InvariantMonoidal, C[_[_]]]
      : InvariantMonoidal[ContextT[F, C, *]] = new ContextTInvariantMonoidalI
}

trait ContextTInstancesW extends ContextTInstancesX {
  final implicit def contextTApplicative[F[+_]: Applicative, C[_[_]]]: Applicative[ContextT[F, C, *]] =
    new ContextTApplicativeI

  final implicit def contextTFlatMap[F[+_]: FlatMap, C[_[_]]]: FlatMap[ContextT[F, C, *]] = new ContextTFlatMapI
}

trait ContextTInstancesV extends ContextTInstancesW {
  final implicit def contextTMonad[F[+_]: Monad, C[_[_]]]: Monad[ContextT[F, C, *]] = new ContextTMonadI

  final implicit def contextTAlternative[F[+_]: Alternative, C[_[_]]]: Alternative[ContextT[F, C, *]] =
    new ContextTAlternativeI

  final implicit def contextTApplicativeError[F[+_]: ApplicativeError[*[_], E], C[_[_]], E]
      : ApplicativeError[ContextT[F, C, *], E] =
    new ContextTApplicativeErrorI
}

trait ContextTInstancesU extends ContextTInstancesV {
  final implicit def contextTMonadError[F[+_]: MonadError[*[_], E], C[_[_]], E]: MonadError[ContextT[F, C, *], E] =
    new ContextTMonadErrorI
}

trait ContextTInstancesT extends ContextTInstancesU {
  final implicit def contextTBracket[F[+_]: Bracket[*[_], E], C[_[_]], E]: Bracket[ContextT[F, C, *], E] =
    new ContextTBracketI
}

trait ContextTInstancesS extends ContextTInstancesT {
  final implicit def contextTSync[F[+_]: Sync, C[_[_]]]: Sync[ContextT[F, C, *]]       = new ContextTSyncI
  final implicit def contextTLiftIO[F[+_]: LiftIO, C[_[_]]]: LiftIO[ContextT[F, C, *]] = new ContextTLiftIOI
}

trait ContextTInstancesR extends ContextTInstancesS {
  final implicit def contextTAsync[F[+_]: Async, C[_[_]]]: Async[ContextT[F, C, *]] = new ContextTAsyncI

  final implicit def contextTContext[F[+_]: Applicative, C[_[_]]]: ContextTContext[F, C] =
    new ContextTContext
}

trait ContextTInstancesQ extends ContextTInstancesR {
  final implicit def contextTConcurrent[F[+_]: Concurrent, C[_[_]]]: Concurrent[ContextT[F, C, *]] =
    new ContextTConcurrentI

  final implicit def contextTRunContext[F[+_]: Applicative: Defer, C[_[_]]]: ContextTRunContext[F, C] =
    new ContextTRunContext

  final def runContextUnsafe[F[+_]: Applicative, C[_[_]]]: HasContextRun[ContextT[F, C, *], F, C[ContextT[F, C, *]]] =
    new ContextTRunContextUnsafe[F, C]
}
