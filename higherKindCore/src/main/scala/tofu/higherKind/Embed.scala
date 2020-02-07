package tofu.higherKind

import cats.FlatMap
import cats.data.{EitherT, IorT, OptionT, ReaderT, WriterT}
import cats.free.Free
import simulacrum.typeclass
import tofu.syntax.monadic._

@typeclass trait Embed[U[_[_]]] {
  def embed[F[_]: FlatMap](ft: F[U[F]]): U[F]
}

object Embed extends EmbedInstanceChain[Embed] {
  def of[U[_[_]], F[_]: FlatMap](fuf: F[U[F]])(implicit embed: Embed[U]): U[F] = embed.embed(fuf)
}

trait EmbedInstanceChain[TC[u[_[_]]] >: Embed[u]] extends RepresentableKInstanceChain[TC] {
  private[this] def freeInstance[A]: Embed[Free[*[_], A]] = new Embed[Free[*[_], A]] {
    def embed[F[_]: FlatMap](ft: F[Free[F, A]]): Free[F, A] = Free.roll(ft)
  }

  private[this] def optionTInstance[A]: Embed[OptionT[*[_], A]] = new Embed[OptionT[*[_], A]] {
    def embed[F[_]: FlatMap](ft: F[OptionT[F, A]]): OptionT[F, A] = OptionT(ft.flatMap(_.value))
  }

  private[this] def eitherTInstance[E, A]: Embed[EitherT[*[_], E, A]] = new Embed[EitherT[*[_], E, A]] {
    def embed[F[_]: FlatMap](ft: F[EitherT[F, E, A]]): EitherT[F, E, A] = EitherT(ft.flatMap(_.value))
  }

  private[this] def writerTInstance[W, A]: Embed[WriterT[*[_], W, A]] = new Embed[WriterT[*[_], W, A]] {
    def embed[F[_]: FlatMap](ft: F[WriterT[F, W, A]]): WriterT[F, W, A] = WriterT(ft.flatMap(_.run))
  }

  private[this] def iorTInstance[E, A]: Embed[IorT[*[_], E, A]] = new Embed[IorT[*[_], E, A]] {
    def embed[F[_]: FlatMap](ft: F[IorT[F, E, A]]): IorT[F, E, A] = IorT(ft.flatMap(_.value))
  }

  private[this] def readerTInstance[R, A]: Embed[ReaderT[*[_], R, A]] = new Embed[ReaderT[*[_], R, A]] {
    def embed[F[_]: FlatMap](ft: F[ReaderT[F, R, A]]): ReaderT[F, R, A] = ReaderT(r => ft.flatMap(_.run(r)))
  }

  private[this] val freeEmbedAny    = freeInstance[Any]
  private[this] val optionTEmbedAny = optionTInstance[Any]
  private[this] val eitherTEmbedAny = eitherTInstance[Any, Any]
  private[this] val writerTEmbedAny = writerTInstance[Any, Any]
  private[this] val iorTEmbedAny    = iorTInstance[Any, Any]
  private[this] val readerTEmbedAny = readerTInstance[Any, Any]

  final implicit def freeEmbed[A]: TC[Free[*[_], A]]             = freeEmbedAny.asInstanceOf[TC[Free[*[_], A]]]
  final implicit def optionEmbed[A]: TC[OptionT[*[_], A]]        = optionTEmbedAny.asInstanceOf[TC[OptionT[*[_], A]]]
  final implicit def eitherTEmbed[E, A]: TC[EitherT[*[_], E, A]] = eitherTEmbedAny.asInstanceOf[TC[EitherT[*[_], E, A]]]
  final implicit def writerTEmbed[W, A]: TC[WriterT[*[_], W, A]] = writerTEmbedAny.asInstanceOf[TC[WriterT[*[_], W, A]]]
  final implicit def iorTEmbed[E, A]: TC[IorT[*[_], E, A]]       = iorTEmbedAny.asInstanceOf[TC[IorT[*[_], E, A]]]
  final implicit def readerTEmbed[R, A]: TC[ReaderT[*[_], R, A]] = readerTEmbedAny.asInstanceOf[TC[ReaderT[*[_], R, A]]]
}
