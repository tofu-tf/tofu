package tofu.higherKind
import cats.data._
import cats.tagless.IdK
import cats.{FlatMap, ~>}
import simulacrum.typeclass
import tofu.syntax.funk.funK
import tofu.syntax.monadic._

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

@typeclass trait RepresentableK[U[_[_]]] extends MonoidalK[U] with Embed[U] {
  def tabulate[F[_]](hom: RepK[U, *] ~> F): U[F]

  override def mapK[F[_], G[_]](af: U[F])(fk: F ~> G): U[G] = tabulate(funK(repr => fk(repr(af))))

  override def productK[F[_], G[_]](af: U[F], ag: U[G]): U[Tuple2K[F, G, *]] =
    tabulate(funK(repr => Tuple2K(repr(af), repr(ag))))

  override def embed[F[_]: FlatMap](ft: F[U[F]]): U[F] = tabulate(funK(repr => ft.flatMap(repr(_))))

  override def zipWith2K[F[_], G[_], H[_]](af: U[F], ag: U[G])(f2: Function2K[F, G, H]): U[H] =
    tabulate(funK(repr => f2(repr(af), repr(ag))))

  override def pureK[F[_]](p: Point[F]): U[F] = tabulate(funK(_ => p.point))
}

object RepresentableK extends RepresentableKInstanceChain[RepresentableK] {

  /** simply for reference
    * continuation form of RepK makes higher order index trivial */
  def index[U[_[_]], F[_], A](tf: U[F])(repr: RepK[U, A]): F[A] = repr(tf)
}

trait RepresentableKInstanceChain[TC[u[_[_]]] >: RepresentableK[u]] {
  private[this] def idKRepresentableInst[A]: RepresentableK[IdK[A]#λ] = new RepresentableK[IdK[A]#λ] {
    def tabulate[F[_]](hom: RepK[IdK[A]#λ, *] ~> F): F[A]                                       = hom(RepK[IdK[A]#λ](x => x))
    override def mapK[F[_], G[_]](af: F[A])(fk: F ~> G): G[A]                                   = fk(af)
    override def productK[F[_], G[_]](af: F[A], ag: G[A]): Tuple2K[F, G, A]                     = Tuple2K(af, ag)
    override def embed[F[_]: FlatMap](ft: F[F[A]]): F[A]                                        = ft.flatten
    override def zipWith2K[F[_], G[_], H[_]](af: F[A], ag: G[A])(f2: Function2K[F, G, H]): H[A] = f2(af, ag)
    override def pureK[F[_]](p: Point[F]): F[A]                                                 = p.point[A]
  }

  private[this] def readerTInstance[R, A]: RepresentableK[ReaderT[*[_], R, A]] =
    new RepresentableK[ReaderT[*[_], R, A]] {
      def tabulate[F[_]](hom: RepK[ReaderT[*[_], R, A], *] ~> F): ReaderT[F, R, A] =
        ReaderT(r => hom(RepK[ReaderT[*[_], R, A]](_.run(r))))
      override def embed[F[_]: FlatMap](ft: F[ReaderT[F, R, A]]): ReaderT[F, R, A]      = ReaderT(r => ft.flatMap(_.run(r)))
      override def pureK[F[_]](p: Point[F]): ReaderT[F, R, A]                           = ReaderT(r => p.point[A])
      override val unitK: ReaderT[UnitK, R, A]                                          = super.unitK
      override def mapK[F[_], G[_]](af: ReaderT[F, R, A])(fk: F ~> G): ReaderT[G, R, A] = af.mapK(fk)

      override def productK[F[_], G[_]](af: ReaderT[F, R, A], ag: ReaderT[G, R, A]): ReaderT[Tuple2K[F, G, *], R, A] =
        ReaderT(r => Tuple2K(af.run(r), ag.run(r)))

      override def zipWith2K[F[_], G[_], H[_]](af: ReaderT[F, R, A], ag: ReaderT[G, R, A])(
          f2: Function2K[F, G, H]
      ): ReaderT[H, R, A] = ReaderT(r => f2(af.run(r), ag.run(r)))
    }

  private[this] def optionTInstance[A]: RepresentableK[OptionT[*[_], A]] = new RepresentableK[OptionT[*[_], A]] {
    def tabulate[F[_]](hom: RepK[OptionT[*[_], A], *] ~> F): OptionT[F, A] = OptionT(
      hom(RepK[OptionT[*[_], A]](_.value))
    )

    override def mapK[F[_], G[_]](af: OptionT[F, A])(fk: F ~> G): OptionT[G, A] = af.mapK(fk)
    override def productK[F[_], G[_]](af: OptionT[F, A], ag: OptionT[G, A]): OptionT[Tuple2K[F, G, *], A] =
      OptionT(Tuple2K(af.value, ag.value))
    override def zipWith2K[F[_], G[_], H[_]](af: OptionT[F, A], ag: OptionT[G, A])(
        f2: Function2K[F, G, H]
    ): OptionT[H, A] =
      OptionT(f2(af.value, ag.value))
    override def pureK[F[_]](p: Point[F]): OptionT[F, A]                   = OptionT(p.point)
    override val unitK: OptionT[UnitK, A]                                  = super.unitK
    override def embed[F[_]: FlatMap](ft: F[OptionT[F, A]]): OptionT[F, A] = OptionT(ft.flatMap(_.value))
  }

  private[this] def eitherTInstance[E, A]: RepresentableK[EitherT[*[_], E, A]] =
    new RepresentableK[EitherT[*[_], E, A]] {
      def tabulate[F[_]](hom: RepK[EitherT[*[_], E, A], *] ~> F): EitherT[F, E, A] =
        EitherT(hom(RepK[EitherT[*[_], E, A]](_.value)))

      override def mapK[F[_], G[_]](af: EitherT[F, E, A])(fk: F ~> G): EitherT[G, E, A] = af.mapK(fk)
      override def productK[F[_], G[_]](af: EitherT[F, E, A], ag: EitherT[G, E, A]): EitherT[Tuple2K[F, G, *], E, A] =
        EitherT(Tuple2K(af.value, ag.value))
      override def zipWith2K[F[_], G[_], H[_]](af: EitherT[F, E, A], ag: EitherT[G, E, A])(
          f2: Function2K[F, G, H]
      ): EitherT[H, E, A] =
        EitherT(f2(af.value, ag.value))
      override def pureK[F[_]](p: Point[F]): EitherT[F, E, A] =
        EitherT(p.point)
      override val unitK: EitherT[UnitK, E, A]                                     = super.unitK
      override def embed[F[_]: FlatMap](ft: F[EitherT[F, E, A]]): EitherT[F, E, A] = EitherT(ft.flatMap(_.value))
    }

  private[this] def writerTInstance[W, A]: RepresentableK[WriterT[*[_], W, A]] =
    new RepresentableK[WriterT[*[_], W, A]] {

      def tabulate[F[_]](hom: RepK[WriterT[*[_], W, A], *] ~> F): WriterT[F, W, A] = WriterT(
        hom(RepK[WriterT[*[_], W, A]](_.run))
      )

      override def mapK[F[_], G[_]](af: WriterT[F, W, A])(fk: F ~> G): WriterT[G, W, A] = af.mapK(fk)
      override def productK[F[_], G[_]](af: WriterT[F, W, A], ag: WriterT[G, W, A]): WriterT[Tuple2K[F, G, *], W, A] =
        WriterT(Tuple2K(af.run, ag.run))
      override def zipWith2K[F[_], G[_], H[_]](af: WriterT[F, W, A], ag: WriterT[G, W, A])(
          f2: Function2K[F, G, H]
      ): WriterT[H, W, A] =
        WriterT(f2(af.run, ag.run))
      override def pureK[F[_]](p: Point[F]): WriterT[F, W, A]                      = WriterT(p.point)
      override val unitK: WriterT[UnitK, W, A]                                     = super.unitK
      override def embed[F[_]: FlatMap](ft: F[WriterT[F, W, A]]): WriterT[F, W, A] = WriterT(ft.flatMap(_.run))
    }

  private[this] def iorTInstance[E, A]: RepresentableK[IorT[*[_], E, A]] = new RepresentableK[IorT[*[_], E, A]] {

    def tabulate[F[_]](hom: RepK[IorT[*[_], E, A], *] ~> F): IorT[F, E, A] = IorT(hom(RepK[IorT[*[_], E, A]](_.value)))

    override def mapK[F[_], G[_]](af: IorT[F, E, A])(fk: F ~> G): IorT[G, E, A] =
      af.mapK(fk)
    override def productK[F[_], G[_]](af: IorT[F, E, A], ag: IorT[G, E, A]): IorT[Tuple2K[F, G, *], E, A] =
      IorT(Tuple2K(af.value, ag.value))
    override def zipWith2K[F[_], G[_], H[_]](af: IorT[F, E, A], ag: IorT[G, E, A])(
        f2: Function2K[F, G, H]
    ): IorT[H, E, A] =
      IorT(f2(af.value, ag.value))
    override def pureK[F[_]](p: Point[F]): IorT[F, E, A]                   = IorT(p.point)
    override val unitK: IorT[UnitK, E, A]                                  = super.unitK
    override def embed[F[_]: FlatMap](ft: F[IorT[F, E, A]]): IorT[F, E, A] = IorT(ft.flatMap(_.value))
  }

  private[this] val optionTRepresentableKAny = optionTInstance[Any]
  private[this] val eitherTRepresentableKAny = eitherTInstance[Any, Any]
  private[this] val writerTRepresentableKAny = writerTInstance[Any, Any]
  private[this] val iorTRepresentableKAny    = iorTInstance[Any, Any]
  private[this] val idKRepresentableAny      = idKRepresentableInst[Any]
  private[this] val readerTInstanceAny       = readerTInstance[Any, Any]

  final implicit def idKRepresentable[A]: TC[IdK[A]#λ] = idKRepresentableAny.asInstanceOf[TC[IdK[A]#λ]]
  final implicit def readerTRepresentable[R, A]: TC[ReaderT[*[_], R, A]] =
    readerTInstanceAny.asInstanceOf[TC[ReaderT[*[_], R, A]]]

  final implicit def optionRepresentableK[A]: TC[OptionT[*[_], A]] =
    optionTRepresentableKAny.asInstanceOf[TC[OptionT[*[_], A]]]
  final implicit def eitherTRepresentableK[E, A]: TC[EitherT[*[_], E, A]] =
    eitherTRepresentableKAny.asInstanceOf[TC[EitherT[*[_], E, A]]]
  final implicit def writerTRepresentableK[W, A]: TC[WriterT[*[_], W, A]] =
    writerTRepresentableKAny.asInstanceOf[TC[WriterT[*[_], W, A]]]
  final implicit def iorTRepresentableK[E, A]: TC[IorT[*[_], E, A]] =
    iorTRepresentableKAny.asInstanceOf[TC[IorT[*[_], E, A]]]

}
