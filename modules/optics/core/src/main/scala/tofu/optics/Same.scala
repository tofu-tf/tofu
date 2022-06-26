package tofu.optics

import tofu.optics.compat.unused

/** polymorphic equality: any relation for S and T equal to relation of A and B */
trait PSame[-S, +T, +A, -B] extends PEquivalent[S, T, A, B] with PBase[PSame, S, T, A, B] {
  self =>

  def rsubst[R[-_, +_]](r: R[A, B]): R[S, T]

  def back(b: B): T                       = rsubst[λ[(`-x`, `+y`) => y]](b)
  def extract(a: S): A                    = inverse.rsubst[λ[(`-x`, `+y`) => y]](a)
  override def inverse: PSame[B, A, T, S] = PSame.invert(this)
}

object Same extends MonoOpticCompanion(PSame) {
  def id[A]: Same[A, A] = PSame.id[A, A]
}

object PSame extends OpticCompanion[PSame] with OpticProduct[PSame] {
  type Context             = OpticContext
  override type Mono[A, B] = Same[A, B]

  @unused private type Inv[-s, +t, +a, -b] = PSame[b, a, t, s]

  private def refl[A, B]: PSame[A, B, A, B] = new PSame[A, B, A, B] {
    def rsubst[K[_, _]](k: K[A, B]): K[A, B] = k
    override def toString                    = "id"
  }

  private val anyId               = refl[Any, Any]
  def id[A, B]: PSame[A, B, A, B] = anyId.asInstanceOf[PSame[A, B, A, B]]

  def compose[S, T, A, B, U, V](f: PSame[A, B, U, V], g: PSame[S, T, A, B]): PSame[S, T, U, V] =
    g.rsubst[PSame[-*, +*, U, V]](f)

  override def product[S1, S2, T1, T2, A1, A2, B1, B2](
      f: PSame[S1, T1, A1, B1],
      g: PSame[S2, T2, A2, B2]
  ): PSame[(S1, S2), (T1, T2), (A1, A2), (B1, B2)] = anyId.asInstanceOf[PSame[(S1, S2), (T1, T2), (A1, A2), (B1, B2)]]

  override def toGeneric[S, T, A, B](o: PSame[S, T, A, B]): Optic[OpticContext, S, T, A, B] =
    new Optic[OpticContext, S, T, A, B] {
      def apply(c: OpticContext)(p: c.P[A, c.F[B]]): c.P[S, c.F[T]] =
        o.rsubst[λ[(`-x`, `+y`) => c.P[x, c.F[y]]]](p)
    }

  override def fromGeneric[S, T, A, B](o: Optic[OpticContext, S, T, A, B]): PSame[S, T, A, B] =
    new PSame[S, T, A, B] {
      def rsubst[R[-_, +_]](r: R[A, B]): R[S, T] =
        o(new OpticContext {
          type F[+x]     = x
          type P[-x, +y] = R[x, y]
        })(r)
    }

  private def invert[S, T, A, B](self: PSame[S, T, A, B]): PSame[B, A, T, S] =
    self.rsubst[Inv[-*, +*, A, B]](PSame.id)

  implicit final class SameOps[A, B](private val s: PSame[A, A, B, B]) extends AnyVal {
    def subst[F[+_]](fa: F[A]): F[B] = s.inverse.rsubst[λ[(`-s`, `+t`) => F[t]]](fa)
  }

  override def delayed[S, T, A, B](o: () => PSame[S, T, A, B]): PSame[S, T, A, B] =
    PSame.anyId.asInstanceOf[PSame[S, T, A, B]]
}
