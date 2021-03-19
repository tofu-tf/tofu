package tofu.syntax

object either {
  implicit final class TofuLeftOps[A, B](private val l: Left[A, B]) extends AnyVal {
    def coerceR[C]: Left[A, C] = l.asInstanceOf[Left[A, C]]
  }

  implicit final class TofuRightOps[A, B](private val l: Right[A, B]) extends AnyVal {
    def coerceL[C]: Right[C, B] = l.asInstanceOf[Right[C, B]]
  }

  implicit final class TofuEitherAssocLOps[A, B, C](private val e: Either[Either[A, B], C]) extends AnyVal {
    def assocL: Either[A, Either[B, C]] = e match {
      case Left(Left(a))  => Left(a)
      case Left(Right(b)) => Right(Left(b))
      case Right(c)       => Right(Right(c))
    }
  }

  implicit final class TofuEitherAssocROps[A, B, C](private val e: Either[A, Either[B, C]]) extends AnyVal {
    def assocR: Either[Either[A, B], C] = e match {
      case Left(a)         => Left(Left(a))
      case Right(Left(b))  => Left(Right(b))
      case Right(Right(c)) => Right(c)
    }
  }

  implicit final class TofuEitherOps[L, R](private val e: Either[L, R]) extends AnyVal {
    def wideLeft[L1 >: L]: Either[L1, R] = e

    def wideRight[R1 >: R]: Either[L, R1] = e
  }
}
