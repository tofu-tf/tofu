package tofu.concurrent

import cats.effect.concurrent.{Deferred, Ref}
import cats.effect.{ContextShift, IO, Timer}
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.parallel._
import cats.syntax.traverse._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.{Ignore, Inside}
import tofu.concurrent.ReadWrite.{ConcurrentReadWrite, _}

import scala.collection.immutable.Queue
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random

@Ignore
class ReadWriteTests extends AsyncWordSpec with Matchers with Inside {

  implicit override def executionContext: ExecutionContext = ExecutionContext.Implicits.global
  implicit val cs: ContextShift[IO]                        = IO.contextShift(executionContext)
  implicit val timer: Timer[IO]                            = IO.timer(executionContext)
  val smallDelay                                           = timer.sleep(20.millis)

  def rwOf(initial: Int, maxReaders: Int = 5): IO[ConcurrentReadWrite[IO, Int]] =
    ReadWrite.of[IO, Int](initial, maxReaders).map(_.asInstanceOf[ConcurrentReadWrite[IO, Int]])

  "ReadWrite" should {

    "acquire read lock when maxReaders is not reached. And then release" in {
      val io = for {
        rw             <- rwOf(1)
        initialState   <- rw.getState
        (xs, releases) <- rw.read.replicateA(4).map(_.unzip)
        blockedState   <- rw.getState
        _              <- releases.parSequence
        releaseState   <- rw.getState
      } yield (initialState, blockedState, releaseState, xs)
      io.unsafeToFuture().map { case (initial, blocked, release, xs) =>
        initial shouldBe State(
          Queue(),
          0,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          1
        )
        blocked shouldBe State(
          Queue(),
          4,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          1
        )
        release shouldBe State(
          Queue(),
          0,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          1
        )
        xs shouldBe List(1, 1, 1, 1)
      }
    }

    "block on read lock" when {

      "maxReaders is exceeded. Wait and then release" in {
        val io = for {
          rw             <- rwOf(1, 2)
          initialState   <- rw.getState
          (i1, release1) <- rw.read
          (i2, release2) <- rw.read
          blockedReader1 <- rw.read.start
          blockedReader2 <- smallDelay *> rw.read.start
          blockedState1  <- smallDelay *> rw.getState
          _              <- release1
          (i3, release3) <- blockedReader1.join
          blockedState2  <- rw.getState
          _              <- release2
          (i4, release4) <- blockedReader2.join
          blockedState3  <- rw.getState
          _              <- (release3, release4).parTupled
          releaseState   <- rw.getState
        } yield (initialState, blockedState1, blockedState2, blockedState3, releaseState, List(i1, i2, i3, i4))
        io.unsafeToFuture().map { case (initial, blocked1, blocked2, blocked3, release, xs) =>
          initial shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blocked1) { case State(q, 2, false, _, _, 1) =>
            q.size shouldBe 2
            q.forall(_.isReader) shouldBe true
          }
          inside(blocked2) { case State(q, 2, false, _, _, 1) =>
            q.size shouldBe 1
            q.head.isReader shouldBe true
          }
          blocked3 shouldBe State(
            Queue(),
            2,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          release shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          xs shouldBe List(1, 1, 1, 1)
        }
      }

      "someone is writing. Wait and then release" in {
        val io = for {
          rw            <- rwOf(1)
          initialState  <- rw.getState
          (i1, update)  <- rw.write
          f             <- rw.read.start
          blockedState  <- smallDelay *> rw.getState
          _             <- update(i1 + 1)
          releaseState1 <- rw.getState
          (i2, release) <- f.join
          _             <- release
          releaseState2 <- rw.getState
        } yield (initialState, blockedState, releaseState1, releaseState2, List(i1, i2))
        io.unsafeToFuture().map { case (initial, blocked, release1, release2, xs) =>
          initial shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blocked) { case State(q, 0, true, _, _, 1) =>
            q.size shouldBe 1
            q.forall(_.isReader) shouldBe true
          }
          release1 shouldBe State(
            Queue(),
            1,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            2
          )
          release2 shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            2
          )
          xs shouldBe List(1, 2)
        }
      }

      "writer in queue. Wait and then release" in {
        val io = for {
          rw             <- rwOf(1)
          initialState   <- rw.getState
          readers        <- rw.read.start.replicateA(3)
          writer1        <- smallDelay *> rw.write.start
          reader4        <- smallDelay *> rw.read.start
          blockedState   <- smallDelay *> rw.getState
          (xs, releases) <- readers.sequence.join.map(_.unzip)
          _              <- releases.parSequence
          writerState    <- rw.getState
          (wi, update)   <- writer1.join
          _              <- update(wi + 1)
          readerState    <- rw.getState
          (ri, release)  <- reader4.join
          _              <- release
          resultState    <- rw.getState
        } yield (initialState, blockedState, writerState, readerState, resultState, xs ::: List(wi, ri))
        io.unsafeToFuture().map { case (initial, blocked, writer, reader, result, xs) =>
          initial shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blocked) { case State(q, 3, false, _, _, 1) =>
            q.size shouldBe 2
            q.map(_.isWriter) shouldBe List(true, false)
          }
          inside(writer) { case State(q, 0, true, _, _, 1) =>
            q.size shouldBe 1
            q.head.isReader shouldBe true
          }
          reader shouldBe State(
            Queue(),
            1,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            2
          )
          result shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            2
          )
          xs shouldBe List(1, 1, 1, 1, 2)
        }
      }

    }

    "acquire write lock when there is no readers or writers. And then release" in {
      val io = for {
        rw           <- rwOf(1)
        initialState <- rw.getState
        (i, update)  <- rw.write
        blockedState <- rw.getState
        _            <- update(i + 1)
        releaseState <- rw.getState
      } yield (initialState, blockedState, releaseState)
      io.unsafeToFuture().map { case (initial, blocked, release) =>
        initial shouldBe State(
          Queue(),
          0,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          1
        )
        blocked shouldBe State(
          Queue(),
          0,
          writeLocked = true,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          1
        )
        release shouldBe State(
          Queue(),
          0,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          2
        )
      }
    }

    "block on write lock" when {

      "someone already writing. Wait and then release" in {
        val io = for {
          rw            <- rwOf(1)
          initialState  <- rw.getState
          (i1, update1) <- rw.write
          f             <- rw.write.start // blocked
          blockedState  <- smallDelay *> rw.getState
          _             <- update1(i1 + 1)
          releaseState1 <- rw.getState
          (i2, update2) <- f.join
          _             <- update2(i2 + 1)
          releaseState2 <- rw.getState
        } yield (initialState, blockedState, releaseState1, releaseState2, List(i1, i2))
        io.unsafeToFuture().map { case (initial, blocked, release1, release2, xs) =>
          initial shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blocked) { case State(q, 0, true, _, _, 1) =>
            q.size shouldBe 1
            q.head.isWriter shouldBe true
          }
          release1 shouldBe State(
            Queue(),
            0,
            writeLocked = true,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            2
          )
          release2 shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            3
          )
          xs shouldBe List(1, 2)
        }
      }

      "someone still reading. Wait and then release" in {
        val io = for {
          rw            <- rwOf(1)
          initialState  <- rw.getState
          (i1, release) <- rw.read
          f             <- rw.write.start // blocked
          blockedState  <- smallDelay *> rw.getState
          _             <- release
          releaseState1 <- rw.getState
          (i2, update2) <- f.join
          _             <- update2(i2 + 1)
          releaseState2 <- rw.getState
        } yield (initialState, blockedState, releaseState1, releaseState2, List(i1, i2))
        io.unsafeToFuture().map { case (initial, blocked, release1, release2, xs) =>
          initial shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blocked) { case State(q, 1, false, _, _, 1) =>
            q.size shouldBe 1
            q.head.isWriter shouldBe true
          }
          release1 shouldBe State(
            Queue(),
            0,
            writeLocked = true,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          release2 shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            2
          )
          xs shouldBe List(1, 1)
        }
      }

    }

    "remove tasks from q" when {

      "cancel on read happened" in {
        val io = for {
          rw            <- rwOf(1, maxReaders = 1)
          initialState  <- rw.getState
          (_, release1) <- rw.read
          f1            <- rw.read.start
          f2            <- rw.read.start
          blockedState  <- smallDelay *> rw.getState
          _             <- f1.cancel
          cancelState   <- rw.getState
          _             <- release1
          (_, release2) <- f2.join
          releaseState1 <- rw.getState
          _             <- release2
          releaseState2 <- rw.getState
        } yield (initialState, blockedState, cancelState, releaseState1, releaseState2)
        io.unsafeToFuture().map { case (initialState, blockedState, cancelState, releaseState1, releaseState2) =>
          initialState shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blockedState) { case State(q, 1, false, tr, _, 1) =>
            q.size shouldBe 2
            q.forall(_.isReader) shouldBe true
            tr.size shouldBe 0
          }
          inside(cancelState) { case State(q, 1, false, tr, _, 1) =>
            q.size shouldBe 2
            q.head.isReader shouldBe true
            tr.size shouldBe 1
          }
          inside(releaseState1) { case State(Queue(), 1, false, tr, _, 1) =>
            tr.isEmpty shouldBe true
          }
          releaseState2 shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
        }
      }

      "timeout on read happened" in {
        val io = for {
          rw           <- rwOf(1)
          initialState <- rw.getState
          fs           <- rw.read.timeout(50.milli).attempt.start.replicateA(10)
          blockedState <- smallDelay *> rw.getState
          _            <- fs.sequence.join
          cancelState  <- rw.getState
        } yield (initialState, blockedState, cancelState)
        io.unsafeToFuture().map { case (initialState, blockedState, cancelState) =>
          initialState shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blockedState) { case State(q, 5, false, tr, _, 1) =>
            q.size shouldBe 5
            q.forall(_.isReader) shouldBe true
            tr.size shouldBe 0
          }
          inside(cancelState) { case State(q, 5, false, tr, _, 1) =>
            q.size shouldBe 5
            tr.size shouldBe 5
          }
        }
      }

      "cancel on write happened" in {
        val io = for {
          rw           <- rwOf(1)
          initialState <- rw.getState
          _            <- rw.read
          f            <- rw.write.start
          _            <- rw.write.start
          blockedState <- smallDelay *> rw.getState
          _            <- f.cancel
          cancelState  <- rw.getState
        } yield (initialState, blockedState, cancelState)
        io.unsafeToFuture().map { case (initial, blocked, cancel) =>
          initial shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blocked) { case State(q, 1, false, _, tw, 1) =>
            q.size shouldBe 2
            q.forall(_.isWriter) shouldBe true
            tw.size shouldBe 0
          }
          inside(cancel) { case State(q, 1, false, _, tw, 1) =>
            q.size shouldBe 2
            q.head.isWriter shouldBe true
            tw.size shouldBe 1
          }
        }
      }

      "timeout on write happened" in {
        val io = for {
          rw           <- rwOf(1)
          initialState <- rw.getState
          fs           <- rw.write.timeout(50.milli).attempt.start.replicateA(10)
          blockedState <- smallDelay *> rw.getState
          _            <- fs.sequence.join
          cancelState  <- rw.getState
        } yield (initialState, blockedState, cancelState)
        io.unsafeToFuture().map { case (initialState, blockedState, cancelState) =>
          initialState shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          inside(blockedState) { case State(q, 0, true, _, tw, 1) =>
            q.size shouldBe 9
            q.forall(_.isWriter) shouldBe true
            tw.size shouldBe 0
          }
          inside(cancelState) { case State(q, 0, true, _, tw, 1) =>
            q.size shouldBe 9
            tw.size shouldBe 9
          }
        }
      }

    }

    "tryRead" when {

      "read is available" in {
        val io = for {
          rw           <- rwOf(1, 1)
          initialState <- rw.getState
          (a, release) <- rw.tryRead.map(_.getOrElse(sys.error("should be some")))
          blockedState <- rw.getState
          _            <- release
          releaseState <- rw.getState
        } yield (initialState, blockedState, releaseState, a)
        io.unsafeToFuture().map { case (initial, blocked, release, i) =>
          initial shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          blocked shouldBe State(
            Queue(),
            1,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          release shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          i shouldBe 1
        }
      }

      "read is unavailable" in {
        val io = for {
          rw           <- rwOf(1, 0)
          initialState <- rw.getState
          non          <- rw.tryRead
          afterState   <- rw.getState
        } yield (initialState, afterState, non)
        io.unsafeToFuture().map { case (initialState, afterState, non) =>
          initialState shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          afterState shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          non shouldBe None
        }
      }

    }

    "tryWrite" when {

      "write is available" in {
        val io = for {
          rw                 <- rwOf(1)
          initialState       <- rw.getState
          (a, putAndRelease) <- rw.tryWrite.map(_.getOrElse(sys.error("should be some")))
          blockedState       <- rw.getState
          _                  <- putAndRelease(a + 1)
          releaseState       <- rw.getState
        } yield (initialState, blockedState, releaseState, a)

        io.unsafeToFuture().map { case (initial, blocked, release, i) =>
          initial shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          blocked shouldBe State(
            Queue(),
            0,
            writeLocked = true,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          release shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            2
          )
          i shouldBe 1
        }
      }

      "write is unavailable" in {
        val io = for {
          rw           <- rwOf(1, 1)
          initialState <- rw.getState
          _            <- rw.read
          non          <- rw.tryWrite
          afterState   <- rw.getState
        } yield (initialState, afterState, non)
        io.unsafeToFuture().map { case (initialState, afterState, non) =>
          initialState shouldBe State(
            Queue(),
            0,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          afterState shouldBe State(
            Queue(),
            1,
            writeLocked = false,
            Set.empty[Deferred[IO, (Int, IO[Unit])]],
            Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
            1
          )
          non shouldBe None
        }
      }

    }

    "read, read, write, write, read, read, write, write" in {
      val io = for {
        rw           <- rwOf(1)
        initialState <- rw.getState
        (i1, r1)     <- rw.read
        rf1          <- rw.read.start
        wf1          <- rw.write.start
        wf2          <- rw.write.start
        rf2          <- rw.read.start
        rf3          <- rw.read.start
        wf3          <- rw.write.start
        wf4          <- rw.write.start
        blockedState <- rw.getState
        (i2, r2)     <- r1 *> rf1.join
        (i3, p1)     <- r2 *> wf1.join
        (i4, p2)     <- p1(i3 + 1) *> wf2.join
        (i5, r3)     <- p2(i4 + 1) *> rf2.join
        (i6, r4)     <- r3 *> rf3.join
        (i7, p3)     <- r4 *> wf3.join
        (i8, p4)     <- p3(i7 + 1) *> wf4.join
        _            <- p4(i8 + 1)
        cleanState   <- rw.getState
      } yield (initialState, blockedState, cleanState, List(i1, i2, i3, i4, i5, i6, i7, i8))
      io.unsafeToFuture().map { case (initial, blocked, clean, is) =>
        initial shouldBe State(
          Queue(),
          0,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          1
        )
        inside(blocked) { case State(q, 2, false, _, _, 1) =>
          q.map(_.isReader) shouldBe List(false, false, true, true, false, false)
        }
        clean shouldBe State(
          Queue(),
          0,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          5
        )
        is shouldBe List(1, 1, 1, 2, 3, 3, 3, 4)
      }
    }

    "write, write, read, read, write, write, read ,read" in {
      val io = for {
        rw           <- rwOf(10)
        initialState <- rw.getState
        (i1, w1)     <- rw.write
        wf1          <- rw.write.start
        rf1          <- rw.read.start
        rf2          <- rw.read.start
        wf2          <- rw.write.start
        wf3          <- rw.write.start
        rf3          <- rw.read.start
        rf4          <- rw.read.start
        blockedState <- rw.getState
        (i2, w2)     <- w1(i1 + 1) *> wf1.join
        (i3, r1)     <- w2(i2 + 1) *> rf1.join
        (i4, r2)     <- rf2.join
        (i5, w3)     <- r1 *> r2 *> wf2.join
        (i6, w4)     <- w3(i5 + 1) *> wf3.join
        (i7, r3)     <- w4(i6 + 1) *> rf3.join
        (i8, r4)     <- rf4.join
        _            <- r3 *> r4
        cleanState   <- rw.getState
      } yield (initialState, blockedState, cleanState, List(i1, i2, i3, i4, i5, i6, i7, i8))

      io.unsafeToFuture().map { case (initial, blocked, clean, is) =>
        initial shouldBe State(
          Queue(),
          0,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          10
        )
        inside(blocked) { case State(q, 0, true, _, _, 10) =>
          q.map(_.isReader) shouldBe List(false, true, true, false, false, true, true)
        }
        clean shouldBe State(
          Queue(),
          0,
          writeLocked = false,
          Set.empty[Deferred[IO, (Int, IO[Unit])]],
          Set.empty[Deferred[IO, (Int, Int => IO[Unit])]],
          14
        )
        is shouldBe List(10, 11, 12, 12, 12, 13, 14, 14)
      }
    }

    "not corrupt data" in {
      type MutableArray = ReadWrite[IO, Vector[Ref[IO, Int]]]

      def reader(arr: MutableArray, isFailed: Ref[IO, Boolean]): IO[Unit] =
        for {
          (values, release) <- arr.read
          vals              <- values.traverse(_.get)
          s                  = vals.sum
          _                 <- if (s != 55) isFailed.set(true) else IO.unit
          _                 <- release
        } yield ()

      def writer(arr: MutableArray): IO[Unit] =
        for {
          (values, put) <- arr.write
          i             <- IO(Random.nextInt(values.length))
          j             <- IO(Random.nextInt(values.length))
          vi            <- values(i).get
          vj            <- values(j).get
          _             <- values(i).set(vj)
          _             <- values(j).set(vi)
          _             <- put(values)
        } yield ()

      val io = for {
        fal <- Ref.of[IO, Boolean](false)
        vec <- Vector.range(1, 11).traverse(i => Ref.of[IO, Int](i))
        arr <- ReadWrite.of[IO, Vector[Ref[IO, Int]]](vec, maxReaders = 5)

        readers   = reader(arr, fal).replicateA(1000).start.replicateA(100) // 100 readers, each will read 1000 times
        writers   = writer(arr).replicateA(1000).start.replicateA(100) // 100 writers, each will swap array 1000 times
        (l1, l2) <- (readers, writers).parTupled

        l3 = l1.sequence
        l4 = l2.sequence
        _ <- l3.join
        _ <- l4.join

        corrupted <- fal.get
      } yield corrupted

      io.unsafeToFuture().map(_ shouldBe false)
    }

  }

}
