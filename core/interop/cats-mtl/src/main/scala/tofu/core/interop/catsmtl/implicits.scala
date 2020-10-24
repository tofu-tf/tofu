package tofu.core.interop.catsmtl

import cats.mtl.{Ask, Local, Handle => MHandle, Raise => MRaise}
import cats.syntax.functor._
import cats.{Applicative, Functor}
import tofu.{Errors, Raise, WithContext, WithLocal}

object implicits {

  implicit def deriveLocal[F[_]: Applicative, C](implicit L: WithLocal[F, C]): Local[F, C] = new Local[F, C] {
    override def local[A](fa: F[A])(f: C => C): F[A] = L.local(fa)(f)

    override def applicative: Applicative[F] = Applicative[F]

    override def ask[E2 >: C]: F[E2] = L.context.map(identity(_: E2))
  }

  implicit def deriveMtlHandle[F[_]: Applicative, E](implicit E: Errors[F, E]): MHandle[F, E] =
    new MHandle[F, E] {
      override def applicative: Applicative[F] = Applicative[F]

      override def handleWith[A](fa: F[A])(f: E => F[A]): F[A] = E.handleWith(fa)(f)

      override def raise[E2 <: E, A](e: E2): F[A] = E.raise[A](e)
    }

  implicit def deriveWithLocal[F[_], C](implicit L: Local[F, C]): WithLocal[F, C] = new WithLocal[F, C] {
    override def local[A](fa: F[A])(project: C => C): F[A] = L.local(fa)(project)

    override def functor: Functor[F] = L.applicative

    override def context: F[C] = L.ask[C]
  }

  implicit def deriveTofuErrors[F[_]: Functor, E](implicit H: MHandle[F, E]): Errors[F, E] = new Errors[F, E] {
    override def raise[A](err: E): F[A] = H.raise[E, A](err)

    override def tryHandleWith[A](fa: F[A])(f: E => Option[F[A]]): F[A] =
      H.handleWith(fa)(e => f(e).getOrElse(H.raise[E, A](e)))

    override def restore[A](fa: F[A]): F[Option[A]] = H.attempt(fa).map(_.toOption)

    override def lift[A](fa: F[A]): F[A] = fa
  }

  implicit def deriveAsk[F[_]: Applicative, C](implicit C: WithContext[F, C]): Ask[F, C] = new Ask[F, C] {
    override def applicative: Applicative[F] = Applicative[F]

    override def ask[E2 >: C]: F[E2] = C.context.map(identity(_: E2))
  }

  implicit def deriveMtlRaise[F[_]: Functor, E](implicit R: Raise[F, E]): MRaise[F, E] = new MRaise[F, E] {
    override def functor: Functor[F] = Functor[F]

    override def raise[E2 <: E, A](e: E2): F[A] = R.raise[A](e)
  }

  implicit def deriveWithContext[F[_], C](implicit A: Ask[F, C]): WithContext[F, C] = new WithContext[F, C] {
    override def functor: Functor[F] = A.applicative

    override def context: F[C] = A.ask[C]
  }

  implicit def deriveTofuRaise[F[_], E](implicit R: MRaise[F, E]): Raise[F, E] = new Raise[F, E] {
    override def raise[A](err: E): F[A] = R.raise[E, A](err)
  }
}
