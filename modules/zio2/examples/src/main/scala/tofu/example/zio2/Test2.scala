package tofu.example.zio2

import zio._

import java.io.IOException

object Test2 extends ZIOAppDefault {

  def logI(i: Int, logger: ULogger) =
    Global.TofuLogContextRef.locally(i)(logger.logMessage(s"from: $i"))


  def tasks(l: ULogger) =
    (100 to 109).map(logI(_, l))

  def run = {
    for {
      logger <- ZIO.service[ULogger]
      _      <- logger.logMessage("1")
      _      <- Global.TofuLogContextRef.update(_ + 1)
      _      <- logger.logMessage("2")
      _ <- ZIO.collectAllParDiscard(tasks(logger))
      _      <- logger.logMessage("end")
    } yield ()
  }.provide(logger)

  val logger = ZLayer.succeed(new ULogger(Global.TofuLogContextRef))
}

class ULogger(ref: FiberRef[Int]) {
  def logMessage(s: String): IO[IOException, Unit] =
    for {
      i <- ref.get
      _ <- Console.printLine(s"Hello! $s\nFiber value: $i")
    } yield ()
}

object Global {
  lazy val TofuLogContextRef = Unsafe.unsafe(implicit u =>
    Runtime.default.unsafe
      .run(ZIO.scoped(FiberRef.make(5)))
      .getOrThrow()
  )
}
