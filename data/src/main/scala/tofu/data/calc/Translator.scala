package tofu.data.calc

import tofu.higherKind.bi.FunBK
import scala.annotation.unchecked.{uncheckedVariance => uv}

trait Translator[-F[_, _], +G[+_, +_], ST, +RI, -RO] { self =>
  def mapRead(ro: RO): RI

  def trans[E, A](fa: F[E, A]): CalcM[G, RO, ST, ST, E, A]

  def translateState[S, E, A](fa: F[E, A]): CalcM[G, RO, (ST, S), (ST, S), E, A] = trans(fa).focusFirst

  def setR[R](r: R): Translator[F, G, ST, R, RO] = new Translator[F, G, ST, R, RO] {
    override def mapRead(ro: RO): R = r

    override def trans[E, A](fa: F[E, A]): CalcM[G, RO, ST, ST, E, A] = self.trans(fa)

    override def setR[R1](r1: R1): Translator[F, G, ST, R1, RO] = self.setR(r1)
  }
}

object Translator {
  type Uno[F[_], E, A] = F[A]

  def apply[F[_, _], R, S]: Applied[F, R, S]       = new Applied
  def uno[F[_], R, S]: Applied[Uno[F, *, *], R, S] = new Applied[Uno[F, *, *], R, S]

  class Applied[F[_, _], R, ST] {
    type E
    type A
    type S

    def apply[G[+_, +_]](mk: AsStateK[F, G, ST, R, E, A]): Translator[F, G, ST, R, R] = mk

    def pure(mk: AsStateK[F, Nothing, ST, R, E, A]): Translator[F, Nothing, ST, R, R] = mk
  }

  abstract class As[-F[_, _], G[+_, +_], ST, R] extends Translator[F, G, ST, R, R] {
    def mapRead(ro: R): R = ro
  }

  abstract class AsStateK[-F[_, _], G[+_, +_], ST, R, E1, A1] extends As[F, G, ST, R] {
    def translateArb(fa: F[E1, A1]): CalcM[G, R, ST, ST, E1, A1]

    def trans[E, A](fa: F[E, A]): CalcM[G, R, ST, ST, E, A] =
      translateArb(fa.asInstanceOf[F[E1, A1]]).asInstanceOf[CalcM[G, R, ST, ST, E, A]]
  }
}
trait ITranslator[-F[_, _], +G[+_, +_], +RI, -RO] extends Translator[F, G, Any, RI, RO] { self =>
  def mapRead(ro: RO): RI
  def translate[S, E, A](fa: F[E, A]): CalcM[G, RO, S, S, E, A]

  def trans[E, A](fa: F[E, A]): CalcM[G, RO, Any, Any, E, A] = translate[Any, E, A](fa)

  override def setR[R](r: R): ITranslator[F, G, R, RO] = new ITranslator[F, G, R, RO] {

    override def mapRead(ro: RO): R = r

    override def translate[S, E, A](fa: F[E, A]): CalcM[G, RO, S, S, E, A] = self.translate(fa)

    override def setR[R1](r1: R1): ITranslator[F, G, R1, RO] = self.setR(r1)
  }
}

object ITranslator {
  def apply[F[_, _], R]: Applied[F, R] = new Applied

  def mapK[F[_, _], G[+_, +_], R](fk: F FunBK G): ITranslator[F, G, R, R] = new ITranslator[F, G, R, R] {
    override def mapRead(ro: R): R = ro

    override def translate[S, E, A](fa: F[E, A]): CalcM[G, R, S, S, E, A] = CalcM.lift(fk(fa))
  }

  def flatMapK[F[_, _], G[+_, +_], R](fk: F FunBK G): ITranslator[F, G, R, R] = new ITranslator[F, G, R, R] {
    override def mapRead(ro: R): R = ro

    override def translate[S, E, A](fa: F[E, A]): CalcM[G, R, S, S, E, A] = CalcM.lift(fk(fa))
  }

  class Applied[F[_, _], R] {
    type E
    type A
    type S

    def mapK[G[+_, +_]](mk: AsMapK[F, G, R, E, A]): ITranslator[F, G, R, R] = mk

    def flatMapK[G[+_, +_]](mk: AsFlatMapK[F, G, R, S, E, A]): ITranslator[F, G, R, R] = mk
  }

  abstract class As[-F[_, _], G[+_, +_], R] extends ITranslator[F, G, R, R] {
    def mapRead(ro: R): R = ro
  }

  abstract class AsMapK[-F[_, _], G[+_, +_], R, E1, A1] extends As[F, G, R] {
    def translateArb(fa: F[E1, A1]): G[E1, A1]

    def translate[S, E, A](fa: F[E, A]): CalcM[G, R, S, S, E, A] =
      CalcM.lift(translateArb(fa.asInstanceOf[F[E1, A1]]).asInstanceOf[G[E, A]])
  }

  abstract class AsFlatMapK[-F[_, _], G[+_, +_], R, S1, E1, A1] extends As[F, G, R] {
    def translateArb(fa: F[E1, A1]): CalcM[G, R, S1, S1, E1, A1]

    def translate[S, E, A](fa: F[E, A]): CalcM[G, R, S, S, E, A] =
      translateArb(fa.asInstanceOf[F[E1, A1]]).asInstanceOf[CalcM[G, R, S, S, E, A]]
  }
}

class TranslatePack[+F[+_, +_], -R, -SI, +SO, +E, +A](private val calc: CalcM[F, R, SI, SO, E, A]) extends AnyVal {
  type EArb
  type AArb
  type SArb
  def mapK[G[+_, +_]](fk: FunBK.Maker[F, G, EArb, AArb]): CalcM[G, R, SI, SO, E, A]                             = calc.mapK(fk)
  def flatMapK[G[+_, +_]](fk: ITranslator.AsFlatMapK[F, G, R @uv, SArb, EArb, AArb]): CalcM[G, R, SI, SO, E, A] =
    calc.translate(fk)

  def state[ST]: TranslateStatePack[F, R, SI, SO, E, A, ST] = new TranslateStatePack(calc)
}

class TranslateStatePack[+F[+_, +_], -R, -SI, +SO, +E, +A, ST](private val calc: CalcM[F, R, SI, SO, E, A])
    extends AnyVal {
  type EArb
  type AArb

  def keep[G[+_, +_]](
      fk: Translator.AsStateK[F, G, ST, R @uv, EArb, AArb]
  ): CalcM[G, R, (ST, SI), (ST, SO), E, A] =
    calc.translateState(fk)

  def apply[G[+_, +_]](
      fk: Translator.AsStateK[F, G, ST, R @uv, EArb, AArb]
  )(implicit s: Unit <:< SI): CalcM[G, R, ST, ST, E, A] =
    CalcM.get[ST].mapState(st => (st, (): SI)) *>> calc.translateState(fk).mapState(_._1)

  def pure(
      fk: Translator.AsStateK[F, Nothing, ST, R @uv, EArb, AArb]
  )(implicit s: Unit <:< SI): CalcM[Nothing, R, ST, ST, E, A] =
    CalcM.get[ST].mapState(st => (st, (): SI)) *>> calc.translateState(fk).mapState(_._1)
}
