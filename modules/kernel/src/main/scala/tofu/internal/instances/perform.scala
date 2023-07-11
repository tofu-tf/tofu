package tofu.internal.instances

import cats.data.{Kleisli, ReaderT}
import cats.tagless.{ContravariantK, FunctorK}
import cats.{Apply, Functor, ~>}
import tofu.PerformOf.Cont
import tofu.lift.{Lift, Unlift}
import tofu.syntax.funk._
import tofu.syntax.monadic._
import tofu.{PerformVia, Performer}
import tofu.kernel.types.PerformOf

final class PerformerContravariantK[F[_], Cancel] extends ContravariantK[({ type L[x[_]] = Performer[F, x, Cancel]})#L] {
  def contramapK[C1[_], C2[_]](af: Performer[F, C1, Cancel])(fk: C2 ~> C1): Performer[F, C2, Cancel] =
    new Performer[F, C2, Cancel] {
      def perform[A](cont: C2[A])(f: F[A]): F[Cancel] = af.perform(fk(cont))(f)
    }
}

final class PerformViaContravariantK[F[_], Cancel] extends ContravariantK[({ type L[x[_]] = PerformVia[F, x, Cancel]})#L] {
  def contramapK[C1[_], C2[_]](af: PerformVia[F, C1, Cancel])(fk: C2 ~> C1) =
    new PerformViaMappedPerformer(af, fk)
}

class PerformViaMappedPerformer[F[_], C1[_], C2[_], Cancel](
    af: PerformVia[F, C1, Cancel],
    fk: C2 ~> C1,
) extends PerformVia[F, C2, Cancel] {
  private[this] val pcontra = Performer.contravariantK[F, Cancel]

  def performer: F[Performer[F, C2, Cancel]] = af.performer.map(pcontra.contramapK(_)(fk))
  implicit def functor: Functor[F]           = af.functor
}

class PerformOfMappedPerformer[F[_], Ex1[_], Ex2[_]](
    af: PerformOf[F, Ex1],
    fk: Ex1 ~> Ex2,
) extends PerformViaMappedPerformer[F, Cont[Ex1, *], Cont[Ex2, *], Unit](
      af,
      funK[Cont[Ex2, *], Cont[Ex1, *]](c1 => ex1 => c1(fk(ex1))),
    ) with PerformOf[F, Ex2]

final class PerformOfFunctorK[F[_]] extends FunctorK[({type L[x[_]] = PerformOf[F, x]})#L] {
  def mapK[Ex1[_], Ex2[_]](af: PerformOf[F, Ex1])(fk: Ex1 ~> Ex2): PerformOf[F, Ex2] =
    new PerformOfMappedPerformer(af, fk)
}

final class ReaderTPerformer[F[_], R, C[_], Cancel](p: Performer[F, C, Cancel], r: R)
    extends Performer[ReaderT[F, R, *], C, Cancel] {
  def perform[A](cont: C[A])(f: Kleisli[F, R, A]): Kleisli[F, R, Cancel] =
    ReaderT.liftF(p.perform(cont)(f.run(r)))
}

final class UnliftPerformer[F[_], B[_], C[_], Cancel](p: Performer[B, C, Cancel], unlifter: F ~> B, lift: Lift[B, F])
    extends Performer[F, C, Cancel] {
  def perform[A](cont: C[A])(f: F[A]): F[Cancel] = lift.lift(p.perform(cont)(unlifter(f)))
}
class PerformViaReader[F[_]: Functor, R, C[_], Cancel](
    p: PerformVia[F, C, Cancel]
) extends PerformVia[ReaderT[F, R, *], C, Cancel] {
  val functor: Functor[ReaderT[F, R, *]] = implicitly

  def performer: ReaderT[F, R, Performer[ReaderT[F, R, *], C, Cancel]] =
    ReaderT(r => p.performer.map(new ReaderTPerformer(_, r)))
}

class PerformViaUnlift[F[_], B[_], C[_], Cancel](implicit
    p: PerformVia[B, C, Cancel],
    unlift: Unlift[B, F],
    val functor: Apply[F]
) extends PerformVia[F, C, Cancel] {

  def performer: F[Performer[F, C, Cancel]] =
    unlift.lift(p.performer).map2(unlift.unlift)(new UnliftPerformer[F, B, C, Cancel](_, _, unlift))
}
