package ru.tinkoff.lol

import cats.Comparison.{EqualTo, GreaterThan, LessThan}
import cats.effect._
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.syntax.concurrent._
import cats.instances.int._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.order._
import cats.{Applicative, FlatMap}
import monix.eval.{Task, TaskApp}
import tofu.env.Env
import cats.effect.syntax.concurrent._

import scala.concurrent.duration._
import scala.io.StdIn
import scala.util.Random

object Apple {
  val config = Config()

  def sleep[F[_]](duration: FiniteDuration)(implicit timer: Timer[F]): F[Unit] = timer.sleep(duration)

  def feedJobs[F[_]: FlatMap: Timer: Console](queue: MVar[F, Unit]): F[Unit] =
    queue.put(()) >> sleep(500.millis) >> feedJobs(queue)

  def run[F[_]: Concurrent: Timer: Console]: F[Unit] =
    for {
      cluster   <- SimpleClusterService[F]
      roller    <- AsyncRoller[F](cluster, config)
      rollerJob <- roller.run.start
      feedJob   <- feedJobs(roller.rollIndex).start
      _         <- Console.putLine("started")
      _         <- sleep(5.seconds)
      _         <- Console.putLine("stopped")
      _         <- feedJob.cancel
      _         <- rollerJob.cancel
    } yield ()
}

object IOApple extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Apple.run[IO] as ExitCode.Success
}

object TaskApple extends TaskApp {
  override def run(args: List[String]): Task[ExitCode] =
    Apple.run[Env[ClusterContext, *]].run(ClusterContext.create("lol")) as ExitCode.Success
}

case class Config(
    template: Template = Template("Lion", """["ogly"]"""),
    indicesCount: Int = 3
)
case class Template(name: String, jsonStr: String)

final case class AsyncRoller[F[_]: Concurrent: Console](cluster: SimpleClusterService[F],
                                                        rollIndex: MVar[F, Unit],
                                                        config: Config)(implicit timer: Timer[F]) {

  def run: F[Unit] = checkTemplates

  private def checkTemplates: F[Unit] =
    Console.putLine("checking templates") >>
      cluster
        .templateExists(config.template.name)
        .ifM(
          ifTrue = checkIndices,
          ifFalse =
            Console.putLine("template does not exist, retrying") >>
              cluster.putTemplate(config.template) >>
              timer.sleep(300.millis) >>
              checkTemplates
        )

  private def checkIndices: F[Unit] = {
    def retry = timer.sleep(300.millis) >> checkIndices

    Console.putLine("checking indices") >>
      cluster.indexCount.map(_ comparison config.indicesCount).flatMap {
        case EqualTo     => Console.putLine("looking for a job") >> lookingForJob
        case LessThan    => Console.putLine("creating index") >> cluster.createIndex >> retry
        case GreaterThan => Console.putLine("removing index") >> cluster.removeStaleIndex >> retry
      }
  }

  private def lookingForJob: F[Unit] =
    (
      rollIndex.take >>
        Console.putLine("received some message") >>
        cluster.createIndex >>
        cluster.removeStaleIndex >>
        cluster.moveAliases >>
        cluster.updateRefreshInterval
    ).attempt.flatMap {
      case Left(_)   => checkIndices
      case Right(()) => lookingForJob
    }
}

object AsyncRoller {
  def apply[F[_]: Concurrent: Timer: Console](cluster: SimpleClusterService[F], config: Config): F[AsyncRoller[F]] =
    for (rollIndex <- MVar[F].empty[Unit])
      yield AsyncRoller(cluster, rollIndex, config)
}

trait SimpleClusterService[F[_]] {
  def templateExists(name: String): F[Boolean]
  def putTemplate(template: Template): F[Unit]
  def indexCount: F[Int]
  def createIndex: F[Unit]
  def removeStaleIndex: F[Unit]
  def moveAliases: F[Unit]
  def updateRefreshInterval: F[Unit]
}

object SimpleClusterService {
  def apply[F[_]: Concurrent]: F[SimpleClusterService[F]] =
    for {
      indxCount   <- Ref[F].of(0)
      tmplExsists <- MVar[F].of(false)
    } yield
      new SimpleClusterService[F] {
        def templateExists(name: String): F[Boolean] = tmplExsists.read
        def putTemplate(template: Template): F[Unit] =
          for {
            _    ← tmplExsists.take
            next ← tmplExsists.put(true)
          } yield next

        def indexCount: F[Int]        = indxCount.get
        def createIndex: F[Unit]      = indxCount.update(_ + 1)
        def removeStaleIndex: F[Unit] = indxCount.update(_ - 1)

        def moveAliases: F[Unit]           = Applicative[F].unit
        def updateRefreshInterval: F[Unit] = Applicative[F].unit
      }
}

case class ClusterContext(trackingId: String, ops: List[String])

object ClusterContext {
  def create(op: String): ClusterContext = ClusterContext(Random.alphanumeric.take(10).mkString, List(op))
}

trait Console[F[_]] {
  def putLine(s: String): F[Unit]
  def readLine(prompt: String): F[String]
}

object Console {
  implicit val ioConsole: Console[IO] = new Console[IO] {
    override def putLine(s: String): IO[Unit]         = IO(println(s))
    override def readLine(prompt: String): IO[String] = IO(StdIn.readLine(prompt + "\n"))
  }

  implicit def envConsole[Ctx]: Console[Env[Ctx, *]] = new Console[Env[Ctx, *]] {
    override def putLine(s: String): Env[Ctx, Unit] =
      Env.fromFunc(ctx => println(s"[$ctx] $s"))
    override def readLine(prompt: String): Env[Ctx, String] =
      Env.fromFunc(ctx => StdIn.readLine(s"[$ctx] : $prompt"))
  }

  def putLine[F[_]](s: String)(implicit console: Console[F]): F[Unit]         = console.putLine(s)
  def readLine[F[_]](prompt: String)(implicit console: Console[F]): F[String] = console.readLine(prompt)
}
