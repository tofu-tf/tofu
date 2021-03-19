package tofu
package concurrent
import cats.{Applicative, Defer}

object ContextTSummonCheck {
  case class Foo[F[_]]()
  def check1[F[+_]: Applicative: Defer] = {

    type I1 = WithLocal[ContextT[F, Foo, *], Foo[ContextT[F, Foo, *]]]
    type I2 = WithRun[ContextT[F, Foo, *], F, Foo[ContextT[F, Foo, *]]]
    implicitly[I1]
    implicitly[I2]
    implicitly[I2 <:< I1]
  }

  def check2[F[+_]: Applicative] = {

    type I1 = WithContext[ContextT[F, Foo, *], Foo[ContextT[F, Foo, *]]]
    implicitly[I1]
  }
}
