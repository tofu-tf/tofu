package tofu.zioInstances

import tofu.WithRun
import tofu.zioInstances.implicits._
import zio._
import zio.test._
import tofu.bi.BiRun

object RunZioSpec extends ZIOSpecDefault {
  case class Context(x: Int)
  private val app: RIO[Context, Int] = ZIO.serviceWith[Context](_.x)
  private val someResult             = 111

  override def spec =
    suite("RunZioSpec")(
      test("should summon and run WithRun instance without errors") {
        for {
          result <- WithRun[RIO[Context, _], Task, Context].runContext(app)(Context(someResult))
        } yield assertTrue(result == someResult)
      },
      test("should summon and run BiRun instance without errors") {
        for {
          result <- BiRun[ZIO[Context, +_, +_], IO[+_, +_], Context, Context].runLeft(app)(Context(someResult))
        } yield assertTrue(result == someResult)
      }
    )
}
