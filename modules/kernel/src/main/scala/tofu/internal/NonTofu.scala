package tofu.internal

/** workaround for scala 2.12.10 derivation */
sealed abstract class IsTofu[+F[_]]

object IsTofu extends IsTofu[Nothing]

/** copied from shapeless.Refute */
sealed abstract class NonTofu[+F[_]]

object NonTofu extends NonTofu[Nothing] {

  /** This results in  ambigous implicits if there is implicit evidence of `IsTofo[F]` */
  implicit def ambiguousIfPresent[F[_]](implicit _ev: IsTofu[F]): NonTofu[F] = this

  /** This always declares an instance of `Refute`
    *
    * This instance will only be found when there is no evidence of `T`
    */
  implicit def refute[F[_]](implicit dummy: DummyImplicit): NonTofu[F] = this
}

/** tag trait to make types more specific to prioritize instances */
trait Prior extends Any
