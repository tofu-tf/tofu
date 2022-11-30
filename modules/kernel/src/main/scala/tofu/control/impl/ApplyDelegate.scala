package tofu.control.impl
import cats.{Apply, Eval, Functor, Invariant, InvariantSemigroupal}

trait ApplyDelegate[F[_]] extends Apply[F] with FunctorDelegate[F] {
  val F: Apply[F]

  final override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = F.ap(ff)(fa)

  final override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]                                        = F.imap(fa)(f)(g)
  final override def productR[A, B](fa: F[A])(fb: F[B]): F[B]                                                = F.productR(fa)(fb)
  final override def productL[A, B](fa: F[A])(fb: F[B]): F[A]                                                = F.productL(fa)(fb)
  final override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]                                            = F.product(fa, fb)
  final override def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z]                              = F.ap2(ff)(fa, fb)
  final override def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z]                                 = F.map2(fa, fb)(f)
  final override def map2Eval[A, B, Z](fa: F[A], fb: Eval[F[B]])(f: (A, B) => Z): Eval[F[Z]]                 = F.map2Eval(fa, fb)(f)
  final override def composeApply[G[_]](implicit G: Apply[G]): InvariantSemigroupal[λ[α => F[G[α]]]]         =
    F.composeApply[G]
  final override def composeFunctor[G[_]](implicit evidence$2: Functor[G]): Invariant[λ[α => F[G[α]]]]       =
    F.composeFunctor
  final override def tuple2[A, B](f1: F[A], f2: F[B]): F[(A, B)]                                             = F.tuple2(f1, f2)
  final override def ap3[A0, A1, A2, Z](f: F[(A0, A1, A2) => Z])(f0: F[A0], f1: F[A1], f2: F[A2]): F[Z]      =
    F.ap3(f)(f0, f1, f2)
  final override def map3[A0, A1, A2, Z](f0: F[A0], f1: F[A1], f2: F[A2])(f: (A0, A1, A2) => Z): F[Z]        =
    F.map3(f0, f1, f2)(f)
  final override def tuple3[A0, A1, A2](f0: F[A0], f1: F[A1], f2: F[A2]): F[(A0, A1, A2)]                    = F.tuple3(f0, f1, f2)
  final override def ap4[A0, A1, A2, A3, Z](
      f: F[(A0, A1, A2, A3) => Z]
  )(f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3]): F[Z] =
    F.ap4(f)(f0, f1, f2, f3)
  final override def map4[A0, A1, A2, A3, Z](f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3])(
      f: (A0, A1, A2, A3) => Z
  ): F[Z] =
    F.map4(f0, f1, f2, f3)(f)
  final override def tuple4[A0, A1, A2, A3](f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3]): F[(A0, A1, A2, A3)] =
    F.tuple4(f0, f1, f2, f3)
  final override def ap5[A0, A1, A2, A3, A4, Z](
      f: F[(A0, A1, A2, A3, A4) => Z]
  )(f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4]): F[Z] = F.ap5(f)(f0, f1, f2, f3, f4)
  final override def map5[A0, A1, A2, A3, A4, Z](f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4])(
      f: (A0, A1, A2, A3, A4) => Z
  ): F[Z] = F.map5(f0, f1, f2, f3, f4)(f)
  final override def tuple5[A0, A1, A2, A3, A4](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4]
  ): F[(A0, A1, A2, A3, A4)] = F.tuple5(f0, f1, f2, f3, f4)
  final override def ap6[A0, A1, A2, A3, A4, A5, Z](
      f: F[(A0, A1, A2, A3, A4, A5) => Z]
  )(f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4], f5: F[A5]): F[Z] = F.ap6(f)(f0, f1, f2, f3, f4, f5)
  final override def map6[A0, A1, A2, A3, A4, A5, Z](f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4], f5: F[A5])(
      f: (A0, A1, A2, A3, A4, A5) => Z
  ): F[Z] = F.map6(f0, f1, f2, f3, f4, f5)(f)
  final override def tuple6[A0, A1, A2, A3, A4, A5](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5]
  ): F[(A0, A1, A2, A3, A4, A5)] = F.tuple6(f0, f1, f2, f3, f4, f5)
  final override def ap7[A0, A1, A2, A3, A4, A5, A6, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6) => Z]
  )(f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4], f5: F[A5], f6: F[A6]): F[Z] =
    F.ap7(f)(f0, f1, f2, f3, f4, f5, f6)
  final override def map7[A0, A1, A2, A3, A4, A5, A6, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6]
  )(f: (A0, A1, A2, A3, A4, A5, A6) => Z): F[Z] = F.map7(f0, f1, f2, f3, f4, f5, f6)(f)
  final override def tuple7[A0, A1, A2, A3, A4, A5, A6](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6]
  ): F[(A0, A1, A2, A3, A4, A5, A6)] = F.tuple7(f0, f1, f2, f3, f4, f5, f6)
  final override def ap8[A0, A1, A2, A3, A4, A5, A6, A7, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7) => Z]
  )(f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4], f5: F[A5], f6: F[A6], f7: F[A7]): F[Z] =
    F.ap8(f)(f0, f1, f2, f3, f4, f5, f6, f7)
  final override def map8[A0, A1, A2, A3, A4, A5, A6, A7, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7) => Z): F[Z] = F.map8(f0, f1, f2, f3, f4, f5, f6, f7)(f)
  final override def tuple8[A0, A1, A2, A3, A4, A5, A6, A7](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7)] = F.tuple8(f0, f1, f2, f3, f4, f5, f6, f7)
  final override def ap9[A0, A1, A2, A3, A4, A5, A6, A7, A8, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8) => Z]
  )(f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4], f5: F[A5], f6: F[A6], f7: F[A7], f8: F[A8]): F[Z] =
    F.ap9(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8)
  final override def map9[A0, A1, A2, A3, A4, A5, A6, A7, A8, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8) => Z): F[Z] = F.map9(f0, f1, f2, f3, f4, f5, f6, f7, f8)(f)
  final override def tuple9[A0, A1, A2, A3, A4, A5, A6, A7, A8](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8)] = F.tuple9(f0, f1, f2, f3, f4, f5, f6, f7, f8)
  final override def ap10[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9]
  ): F[Z] = F.ap10(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9)
  final override def map10[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z): F[Z] = F.map10(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9)(f)
  final override def tuple10[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] = F.tuple10(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9)
  final override def ap11[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10]
  ): F[Z] = F.ap11(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
  final override def map11[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z): F[Z] =
    F.map11(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)(f)
  final override def tuple11[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] = F.tuple11(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
  final override def ap12[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11]
  ): F[Z] = F.ap12(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
  final override def map12[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z): F[Z] =
    F.map12(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)(f)
  final override def tuple12[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] =
    F.tuple12(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
  final override def ap13[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12]
  ): F[Z] = F.ap13(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
  final override def map13[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z): F[Z] =
    F.map13(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)(f)
  final override def tuple13[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] =
    F.tuple13(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
  final override def ap14[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13]
  ): F[Z] = F.ap14(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
  final override def map14[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z): F[Z] =
    F.map14(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)(f)
  final override def tuple14[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] =
    F.tuple14(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
  final override def ap15[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14]
  ): F[Z] = F.ap15(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
  final override def map15[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z): F[Z] =
    F.map15(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)(f)
  final override def tuple15[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] =
    F.tuple15(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
  final override def ap16[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15]
  ): F[Z] = F.ap16(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
  final override def map16[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z): F[Z] =
    F.map16(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)(f)
  final override def tuple16[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] =
    F.tuple16(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
  final override def ap17[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16]
  ): F[Z] = F.ap17(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
  final override def map17[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z): F[Z] =
    F.map17(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)(f)
  final override def tuple17[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] =
    F.tuple17(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
  final override def ap18[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17]
  ): F[Z] = F.ap18(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
  final override def map18[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z): F[Z] =
    F.map18(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)(f)
  final override def tuple18[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] =
    F.tuple18(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
  final override def ap19[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18]
  ): F[Z] = F.ap19(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)
  final override def map19[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z): F[Z] =
    F.map19(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)(f)
  final override def tuple19[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] =
    F.tuple19(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)
  final override def ap20[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19]
  ): F[Z] = F.ap20(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)
  final override def map20[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z): F[Z] =
    F.map20(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)(f)
  final override def tuple20[
      A0,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19
  ](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] =
    F.tuple20(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)
  final override def ap21[
      A0,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      Z
  ](
      f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z]
  )(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19],
      f20: F[A20]
  ): F[Z] =
    F.ap21(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)
  final override def map21[
      A0,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      Z
  ](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19],
      f20: F[A20]
  )(f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z): F[Z] =
    F.map21(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)(f)
  final override def tuple21[
      A0,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20
  ](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19],
      f20: F[A20]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] =
    F.tuple21(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)
  final override def ap22[
      A0,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      Z
  ](f: F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z])(
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19],
      f20: F[A20],
      f21: F[A21]
  ): F[Z] =
    F.ap22(f)(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21)
  final override def map22[
      A0,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      Z
  ](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19],
      f20: F[A20],
      f21: F[A21]
  )(
      f: (A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z
  ): F[Z] =
    F.map22(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21)(f)
  final override def tuple22[
      A0,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21
  ](
      f0: F[A0],
      f1: F[A1],
      f2: F[A2],
      f3: F[A3],
      f4: F[A4],
      f5: F[A5],
      f6: F[A6],
      f7: F[A7],
      f8: F[A8],
      f9: F[A9],
      f10: F[A10],
      f11: F[A11],
      f12: F[A12],
      f13: F[A13],
      f14: F[A14],
      f15: F[A15],
      f16: F[A16],
      f17: F[A17],
      f18: F[A18],
      f19: F[A19],
      f20: F[A20],
      f21: F[A21]
  ): F[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] =
    F.tuple22(f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21)

}
