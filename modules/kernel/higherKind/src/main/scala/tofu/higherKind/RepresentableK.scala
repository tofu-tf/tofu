package tofu.higherKind
import cats.data._
import cats.tagless.IdK
import cats.{FlatMap, ~>}
import tofu.syntax.funk
import tofu.syntax.monadic._

import tofu.internal.EffectCompHK

trait RepK[U[_[_]], A] {
  def apply[R[_]](ar: U[R]): R[A]
}

object RepK {
  def apply[U[_[_]]] = new Applied[U](true)
  def mk[U[_[_]]]    = new Applied[U](true)

  class Applied[T[_[_]]](private val __ : Boolean) extends AnyVal {
    type Arb[_]
    def apply[A](maker: MakeRepr[T, A, Arb]): RepK[T, A] = maker
  }

  abstract class MakeRepr[T[_[_]], A, Arb[_]] extends RepK[T, A] {
    def applyArbitrary(fk: T[Arb]): Arb[A]

    def apply[F[_]](fk: T[F]): F[A] = applyArbitrary(fk.asInstanceOf[T[Arb]]).asInstanceOf[F[A]]
  }
}

trait RepresentableK[U[_[_]]] extends MonoidalK[U] with Embed[U] {
  import RepresentableK.Tab
  def tabulate[F[_]](hom: RepK[U, _] ~> F): U[F]

  final def tab[F[_]]: Tab[U, F] = new Tab(this)

  override def mapK[F[_], G[_]](af: U[F])(fk: F ~> G): U[G] = tab[G](repr => fk(repr(af)))

  override def productK[F[_], G[_]](af: U[F], ag: U[G]): U[Tuple2K[F, G, _]] =
    tab[Tuple2K[F, G, _]](repr => Tuple2K(repr(af), repr(ag)))

  override def embed[F[_]: FlatMap](ft: F[U[F]]): U[F] = tab[F](repr => ft.flatMap(repr(_)))

  override def zipWith2K[F[_], G[_], H[_]](af: U[F], ag: U[G])(f2: Function2K[F, G, H]): U[H] =
    tab[H](repr => f2(repr(af), repr(ag)))

  override def pureK[F[_]](p: Point[F]): U[F] = tab[F](_ => p.point)
}

object RepresentableK extends RepresentableKInstanceChain[RepresentableK] with EffectCompHK[RepresentableK] {
  class Tab[U[f[_]], F[_]](private val rep: RepresentableK[U]) extends AnyVal {
    type A1
    def apply(maker: funk.Maker[RepK[U, _], F, A1]): U[F] = rep.tabulate(maker)
  }

  /** simply for reference continuation form of RepK makes higher order index trivial
    */
  def index[U[_[_]], F[_], A](tf: U[F])(repr: RepK[U, A]): F[A] = repr(tf)
}

trait RepresentableKInstanceChain[TC[u[_[_]]] >: RepresentableK[u]] {
  private[this] def idKRepresentableInst[A]: RepresentableK[IdK[A]#λ] = new RepresentableK[IdK[A]#λ] {
    def tabulate[F[_]](hom: RepK[IdK[A]#λ, _] ~> F): F[A]                                       = hom(RepK[IdK[A]#λ](x => x))
    override def mapK[F[_], G[_]](af: F[A])(fk: F ~> G): G[A]                                   = fk(af)
    override def productK[F[_], G[_]](af: F[A], ag: G[A]): Tuple2K[F, G, A]                     = Tuple2K(af, ag)
    override def embed[F[_]: FlatMap](ft: F[F[A]]): F[A]                                        = ft.flatten
    override def zipWith2K[F[_], G[_], H[_]](af: F[A], ag: G[A])(f2: Function2K[F, G, H]): H[A] = f2(af, ag)
    override def pureK[F[_]](p: Point[F]): F[A]                                                 = p.point[A]
  }

  private[this] def readerTInstance[R, A]: RepresentableK[({ type L[x[_]] = ReaderT[x, R, A] })#L] =
    new RepresentableK[({ type L[x[_]] = ReaderT[x, R, A] })#L] {
      type LReaderT[x[_]] = ReaderT[x, R, A]
      def tabulate[F[_]](hom: RepK[LReaderT, _] ~> F): ReaderT[F, R, A]                 =
        ReaderT(r => hom(RepK[LReaderT](_.run(r))))
      override def embed[F[_]: FlatMap](ft: F[ReaderT[F, R, A]]): ReaderT[F, R, A]      = ReaderT(r => ft.flatMap(_.run(r)))
      override def pureK[F[_]](p: Point[F]): ReaderT[F, R, A]                           = ReaderT(_ => p.point[A])
      override val unitK: ReaderT[UnitK, R, A]                                          = super.unitK
      override def mapK[F[_], G[_]](af: ReaderT[F, R, A])(fk: F ~> G): ReaderT[G, R, A] = af.mapK(fk)

      override def productK[F[_], G[_]](af: ReaderT[F, R, A], ag: ReaderT[G, R, A]): ReaderT[Tuple2K[F, G, _], R, A] =
        ReaderT(r => Tuple2K(af.run(r), ag.run(r)))

      override def zipWith2K[F[_], G[_], H[_]](af: ReaderT[F, R, A], ag: ReaderT[G, R, A])(
          f2: Function2K[F, G, H]
      ): ReaderT[H, R, A] = ReaderT(r => f2(af.run(r), ag.run(r)))
    }

  private[this] def optionTInstance[A]: RepresentableK[({ type L[x[_]] = OptionT[x, A] })#L] =
    new RepresentableK[({ type L[x[_]] = OptionT[x, A] })#L] {
      def tabulate[F[_]](hom: RepK[({ type L[x[_]] = OptionT[x, A] })#L, _] ~> F): OptionT[F, A] = OptionT(
        hom(RepK[({ type L[x[_]] = OptionT[x, A] })#L](_.value))
      )

      override def mapK[F[_], G[_]](af: OptionT[F, A])(fk: F ~> G): OptionT[G, A]                           = af.mapK(fk)
      override def productK[F[_], G[_]](af: OptionT[F, A], ag: OptionT[G, A]): OptionT[Tuple2K[F, G, _], A] =
        OptionT(Tuple2K(af.value, ag.value))
      override def zipWith2K[F[_], G[_], H[_]](af: OptionT[F, A], ag: OptionT[G, A])(
          f2: Function2K[F, G, H]
      ): OptionT[H, A] =
        OptionT(f2(af.value, ag.value))
      override def pureK[F[_]](p: Point[F]): OptionT[F, A]                                                  = OptionT(p.point)
      override val unitK: OptionT[UnitK, A]                                                                 = super.unitK
      override def embed[F[_]: FlatMap](ft: F[OptionT[F, A]]): OptionT[F, A]                                = OptionT(ft.flatMap(_.value))
    }

  private[this] def eitherTInstance[E, A]: RepresentableK[({ type L[x[_]] = EitherT[x, E, A] })#L] =
    new RepresentableK[({ type L[x[_]] = EitherT[x, E, A] })#L] {
      def tabulate[F[_]](hom: RepK[({ type L[x[_]] = EitherT[x, E, A] })#L, _] ~> F): EitherT[F, E, A] =
        EitherT(hom(RepK[({ type L[x[_]] = EitherT[x, E, A] })#L](_.value)))

      override def mapK[F[_], G[_]](af: EitherT[F, E, A])(fk: F ~> G): EitherT[G, E, A]                              = af.mapK(fk)
      override def productK[F[_], G[_]](af: EitherT[F, E, A], ag: EitherT[G, E, A]): EitherT[Tuple2K[F, G, _], E, A] =
        EitherT(Tuple2K(af.value, ag.value))
      override def zipWith2K[F[_], G[_], H[_]](af: EitherT[F, E, A], ag: EitherT[G, E, A])(
          f2: Function2K[F, G, H]
      ): EitherT[H, E, A] =
        EitherT(f2(af.value, ag.value))
      override def pureK[F[_]](p: Point[F]): EitherT[F, E, A]                                                        =
        EitherT(p.point)
      override val unitK: EitherT[UnitK, E, A]                                                                       = super.unitK
      override def embed[F[_]: FlatMap](ft: F[EitherT[F, E, A]]): EitherT[F, E, A]                                   = EitherT(ft.flatMap(_.value))
    }

  private[this] def writerTInstance[W, A]: RepresentableK[({ type L[x[_]] = WriterT[x, W, A] })#L] =
    new RepresentableK[({ type L[x[_]] = WriterT[x, W, A] })#L] {

      def tabulate[F[_]](hom: RepK[({ type L[x[_]] = WriterT[x, W, A] })#L, _] ~> F): WriterT[F, W, A] = WriterT(
        hom(RepK[({ type L[x[_]] = WriterT[x, W, A] })#L](_.run))
      )

      override def mapK[F[_], G[_]](af: WriterT[F, W, A])(fk: F ~> G): WriterT[G, W, A]                              = af.mapK(fk)
      override def productK[F[_], G[_]](af: WriterT[F, W, A], ag: WriterT[G, W, A]): WriterT[Tuple2K[F, G, _], W, A] =
        WriterT(Tuple2K(af.run, ag.run))
      override def zipWith2K[F[_], G[_], H[_]](af: WriterT[F, W, A], ag: WriterT[G, W, A])(
          f2: Function2K[F, G, H]
      ): WriterT[H, W, A] =
        WriterT(f2(af.run, ag.run))
      override def pureK[F[_]](p: Point[F]): WriterT[F, W, A]                                                        = WriterT(p.point)
      override val unitK: WriterT[UnitK, W, A]                                                                       = super.unitK
      override def embed[F[_]: FlatMap](ft: F[WriterT[F, W, A]]): WriterT[F, W, A]                                   = WriterT(ft.flatMap(_.run))
    }

  private[this] def iorTInstance[E, A]: RepresentableK[({ type L[x[_]] = IorT[x, E, A] })#L] =
    new RepresentableK[({ type L[x[_]] = IorT[x, E, A] })#L] {

      def tabulate[F[_]](hom: RepK[({ type L[x[_]] = IorT[x, E, A] })#L, _] ~> F): IorT[F, E, A] = IorT(
        hom(RepK[({ type L[x[_]] = IorT[x, E, A] })#L](_.value))
      )

      override def mapK[F[_], G[_]](af: IorT[F, E, A])(fk: F ~> G): IorT[G, E, A]                           =
        af.mapK(fk)
      override def productK[F[_], G[_]](af: IorT[F, E, A], ag: IorT[G, E, A]): IorT[Tuple2K[F, G, _], E, A] =
        IorT(Tuple2K(af.value, ag.value))
      override def zipWith2K[F[_], G[_], H[_]](af: IorT[F, E, A], ag: IorT[G, E, A])(
          f2: Function2K[F, G, H]
      ): IorT[H, E, A] =
        IorT(f2(af.value, ag.value))
      override def pureK[F[_]](p: Point[F]): IorT[F, E, A]                                                  = IorT(p.point)
      override val unitK: IorT[UnitK, E, A]                                                                 = super.unitK
      override def embed[F[_]: FlatMap](ft: F[IorT[F, E, A]]): IorT[F, E, A]                                = IorT(ft.flatMap(_.value))
    }

  private[this] val optionTRepresentableKAny = optionTInstance[Any]
  private[this] val eitherTRepresentableKAny = eitherTInstance[Any, Any]
  private[this] val writerTRepresentableKAny = writerTInstance[Any, Any]
  private[this] val iorTRepresentableKAny    = iorTInstance[Any, Any]
  private[this] val idKRepresentableAny      = idKRepresentableInst[Any]
  private[this] val readerTInstanceAny       = readerTInstance[Any, Any]

  final implicit def idKRepresentable[A]: TC[IdK[A]#λ]                                       = idKRepresentableAny.asInstanceOf[TC[IdK[A]#λ]]
  final implicit def readerTRepresentable[R, A]: TC[({ type L[x[_]] = ReaderT[x, R, A] })#L] =
    readerTInstanceAny.asInstanceOf[TC[({ type L[x[_]] = ReaderT[x, R, A] })#L]]

  final implicit def optionRepresentableK[A]: TC[({ type L[x[_]] = OptionT[x, A] })#L]        =
    optionTRepresentableKAny.asInstanceOf[TC[({ type L[x[_]] = OptionT[x, A] })#L]]
  final implicit def eitherTRepresentableK[E, A]: TC[({ type L[x[_]] = EitherT[x, E, A] })#L] =
    eitherTRepresentableKAny.asInstanceOf[TC[({ type L[x[_]] = EitherT[x, E, A] })#L]]
  final implicit def writerTRepresentableK[W, A]: TC[({ type L[x[_]] = WriterT[x, W, A] })#L] =
    writerTRepresentableKAny.asInstanceOf[TC[({ type L[x[_]] = WriterT[x, W, A] })#L]]
  final implicit def iorTRepresentableK[E, A]: TC[({ type L[x[_]] = IorT[x, E, A] })#L]       =
    iorTRepresentableKAny.asInstanceOf[TC[({ type L[x[_]] = IorT[x, E, A] })#L]]

}
