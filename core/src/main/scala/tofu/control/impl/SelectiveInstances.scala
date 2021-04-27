package tofu.control.impl
import cats.data.{EitherT, OptionT, ReaderT, WriterT}
import cats.instances.either._
import cats.instances.option._
import cats.{Applicative, Monad, Monoid}
import tofu.control.Selective
import tofu.syntax.either._
import tofu.syntax.monadic._

class SelectiveOverMonad[F[_]](implicit val F: Monad[F]) extends Selective[F] with ApplicativeDelegate[F] {
  def selectAp[A, B](fe: F[Either[A, B]])(ff: => F[A => B]): F[B] = F.flatMap(fe) {
    case Left(a)  => F.map(ff)(_(a))
    case Right(b) => F.pure(b)
  }

  override def select[A](fo: F[Option[A]])(fa: => F[A]): F[A] = F.flatMap(fo) {
    case None    => fa
    case Some(a) => F.pure(a)
  }

  override def whens[A](fb: F[Boolean])(fa: => F[A]): F[Option[A]]   = F.flatMap(fb) {
    if (_) F.map(fa)(Some(_)) else F.pure(None)
  }
  override def unlesss[A](fb: F[Boolean])(fa: => F[A]): F[Option[A]] = F.flatMap(fb) {
    if (_) F.pure(None) else F.map(fa)(Some(_))
  }
  override def whens_[A](fb: F[Boolean])(fa: => F[A]): F[Unit]       = F.flatMap(fb) {
    if (_) F.void(fa) else F.unit
  }
  override def unlesss_[A](fb: F[Boolean])(fa: => F[A]): F[Unit]     = F.flatMap(fb) {
    if (_) F.unit else F.void(fa)
  }
}

/** Implement `ap` method via `selectAp` and `map` */
trait SelectiveWithMap[F[_]] extends Selective[F] {
  def smap[A, B](fa: F[A])(f: A => B): F[B]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    selectAp[A, B](smap(fa)(Left(_)))(ff)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = smap(fa)(f)
}

class SelectiveOptionT[F[_]](implicit F: Selective[F]) extends Selective[OptionT[F, *]] {
  def selectAp[A, B](fe: OptionT[F, Either[A, B]])(ff: => OptionT[F, A => B]): OptionT[F, B] =
    OptionT(F.selectAp(fe.value.map {
      case Some(Left(a))  => Left(a)
      case Some(Right(b)) => Right(Some(b))
      case None           => Right(None)
    })(ff.value.map {
      case Some(f) => b => Some(f(b))
      case None    => _ => None
    }))

  def ap[A, B](ff: OptionT[F, A => B])(fa: OptionT[F, A]): OptionT[F, B] =
    OptionT(F.ap[Option[A], Option[B]](ff.value.map(fo => (fo ap _)))(fa.value))

  def pure[A](x: A): OptionT[F, A] = OptionT.pure(x)
}

class SelectiveEitherT[F[_], L](implicit F: Selective[F]) extends Selective[EitherT[F, L, *]] {
  def selectAp[A, B](fe: EitherT[F, L, Either[A, B]])(ff: => EitherT[F, L, A => B]): EitherT[F, L, B] =
    EitherT(F.selectAp[A, Either[L, B]](fe.value.map {
      case Right(Left(a))       => Left(a)
      case rr @ Right(Right(_)) => rr.asInstanceOf[Either[A, Either[L, B]]]
      case l @ Left(_)          => Right(l.coerceR[B])
    })(ff.value.map {
      case Right(f)    => b => Right(f(b))
      case l @ Left(_) => _ => l.coerceR[B]
    }))

  def ap[A, B](ff: EitherT[F, L, A => B])(fa: EitherT[F, L, A]): EitherT[F, L, B] =
    EitherT(F.ap[Either[L, A], Either[L, B]](ff.value.map(fo => (fo ap _)))(fa.value))

  def pure[A](x: A): EitherT[F, L, A] = EitherT.pure(x)
}

class SelectiveReaderT[F[_], R](implicit FS: Selective[F])
    extends Selective[ReaderT[F, R, *]] with ApplicativeDelegate[ReaderT[F, R, *]] {
  val F: Applicative[ReaderT[F, R, *]] = implicitly

  def selectAp[A, B](fe: ReaderT[F, R, Either[A, B]])(ff: => ReaderT[F, R, A => B]): ReaderT[F, R, B] =
    ReaderT(r => FS.selectAp(fe.run(r))(ff.run(r)))
}

class SelectiveWriterT[F[_], W: Monoid](implicit FS: Selective[F])
    extends Selective[WriterT[F, W, *]] with ApplicativeDelegate[WriterT[F, W, *]] {
  val F: Applicative[WriterT[F, W, *]] = implicitly

  def selectAp[A, B](fe: WriterT[F, W, Either[A, B]])(ff: => WriterT[F, W, A => B]): WriterT[F, W, B] =
    WriterT(FS.selectAp[(W, A), (W, B)](FS.map(fe.run) {
      case (w, Left(a))  => Left((w, a))
      case (w, Right(b)) => Right((w, b))
    })(FS.map(ff.run)(wf => (wf ap _))))
}
