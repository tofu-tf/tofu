package tofu
import cats.Functor
import cats.data.ReaderT
import cats.tagless.ContravariantK
import tofu.internal.carriers.{PerformCarrier2, PerformCarrier2Context, PerformCarrier3}
import tofu.internal.instances._
import tofu.internal.{Effect2Comp, EffectComp}
import tofu.kernel.types.{PerformCont, PerformExitCont, PerformOf, PerformThrow}
import scala.concurrent.Promise
import tofu.concurrent.Exit
import scala.concurrent.Future
import scala.annotation.implicitNotFound

trait Performer[F[_], -Cont[_], Cancel] {
  def perform[A](cont: Cont[A])(fa: F[A]): F[Cancel]

  def toFuture[A](fa: F[A])(implicit ev: (Exit[Throwable, A] => Unit) <:< Cont[A]): (Future[A], F[Cancel]) = {
    val p                                = Promise[A]()
    val cont: Exit[Throwable, A] => Unit = ex => p.complete(ex.toTry)

    (p.future, perform[A](cont)(fa))
  }
}

object Performer {
  type OfExit[F[_], E] = Performer[F, PerformOf.ExitCont[E, *], Unit]

  implicit def contravariantK[F[_], Cancel]: ContravariantK[({type L[x[_]] = Performer[F, x, Cancel]})#L] =
    new PerformerContravariantK[F, Cancel]
}

@implicitNotFound("""can not find Perform instance for functor ${F} 
with continuation ${Cont} and cancel result ${Cancel}
if you are using cats-effect 3.0 make sure you have an implicit instance of WithContext[F, Dispatcher[F]] 
or in case of F[A] = ReaderT[G, C, A], you have an instance of WithContext[F, Dispatcher[G]]""")
trait PerformVia[F[_], Cont[_], Cancel] extends WithContext[F, Performer[F, Cont, Cancel]] {
  def performer: F[Performer[F, Cont, Cancel]]
  final def context: F[Performer[F, Cont, Cancel]] = performer
}

object PerformVia extends PerformInterop {
  def apply[F[_], Cont[_], Cancel](implicit instance: PerformVia[F, Cont, Cancel]): PerformVia[F, Cont, Cancel] =
    instance

  implicit def contravariantK[F[_], Cancel]: ContravariantK[({ type L[x[_]] = PerformVia[F, x[_], Cancel]})#L] =
    new PerformViaContravariantK[F, Cancel]

  implicit def performReader[F[_]: Functor, Cont[_], R, Cancel](implicit
      RP: PerformVia[F, Cont, Cancel]
  ): PerformVia[ReaderT[F, R, *], Cont, Cancel] = new PerformViaReader(RP)
}

class PerformInterop  extends PerformInterop1 {
  final implicit def interopCE3[F[_]](implicit carrier: PerformCarrier3[F]): PerformThrow[F] = carrier
}
class PerformInterop1 extends PerformInterop2 {
  final implicit def interopCE2[F[_]](implicit carrier: PerformCarrier2[F]): PerformThrow[F] = carrier
}

class PerformInterop2 {
  final implicit def interopCE2Contextual[F[_]](implicit
      carrier: PerformCarrier2Context[F]
  ): PerformThrow[F] = carrier
}

object PerformOf extends Effect2Comp[PerformOf] {
  type Cont[Ex[_], A] = PerformCont[Ex, A]
  type ExitCont[E, A] = PerformExitCont[E, A]
}

object PerformThrow extends EffectComp[PerformThrow]
