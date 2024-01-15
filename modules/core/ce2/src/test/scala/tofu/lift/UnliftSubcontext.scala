package tofu.lift

import cats.data.ReaderT
import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import tofu.lift.UnliftSubcontext.FatApp
import glass.Contains
import UnliftSubcontext._
import cats.Monad
import org.scalatest.matchers.should.Matchers
import tofu.HasContext
import tofu.compat.unused

class UnliftSubcontext extends AnyFlatSpec with Matchers {
  def context[F[_]: ({ type L[x[_]] = HasContext[x, Big] })#L]: F[Big]      = HasContext[F, Big].context
  def smallCtx[F[_]: ({ type L[x[_]] = HasContext[x, Small] })#L]: F[Small] = HasContext[F, Small].context

  implicit val ul: Unlift[App, FatApp] = Unlift.subContextUnlift
  val init: Big                        = Big(0, Small(1))
  val sub: Small                       = Small(2)

  val fatAppProgram: IO[Big] =
    context[FatApp].run(init)

  val fatAppWithSubContextReplace: IO[Big] =
    Unlift[App, FatApp].unlift
      .flatMapF(fg => fg(context[FatApp]).run(sub))
      .run(init)

  "Unlift" should "correctly replace subcontext" in {
    fatAppProgram.unsafeRunSync() shouldBe init
    fatAppWithSubContextReplace.unsafeRunSync() shouldBe init.copy(small = sub)
  }

  "Unlift" should "correctly lift" in {
    smallCtx[App].run(sub).unsafeRunSync() shouldBe sub
    Unlift[App, FatApp].lift(smallCtx[App]).run(Big(0, sub)).unsafeRunSync() shouldBe sub
  }
}

object UnliftSubcontext {
  case class Big(y: Int, small: Small)
  case class Small(x: Int)
  implicit val lens: Big Contains Small = Contains[Big](_.small)((big, small) => big.copy(small = small))

  type App[A]    = ReaderT[IO, Small, A]
  type FatApp[A] = ReaderT[IO, Big, A]

  def summonUnliftSubContext[F[_]: Monad](): Unit = {
    @unused
    val ul: Unlift[ReaderT[F, Small, _], ReaderT[F, Big, _]] = Unlift.subContextUnlift
    ()
  }
}
