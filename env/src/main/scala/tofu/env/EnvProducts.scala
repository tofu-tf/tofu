package tofu.env

import cats.arrow.FunctionK
import cats.{Id, ~>}
import monix.eval.{Coeval, Task}

import scala.util.Try
private[env] trait EnvProducts {
  self: Env.type =>
  def parMap2[E, A1, A2, R](fa1: Env[E, A1], fa2: Env[E, A2])(f: (A1, A2) => R): Env[E, R] =
    (fa1, fa2) match {
      case (EnvTask(ta1), EnvTask(ta2)) => EnvTask(Task.parMap2(ta1, ta2)(f))
      case _                            => Env(ctx => Task.parMap2(fa1.run(ctx), fa2.run(ctx))(f))
    }

  def parMap3[E, A1, A2, A3, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3])(f: (A1, A2, A3) => R): Env[E, R] =
    (fa1, fa2, fa3) match {
      case (EnvTask(ta1), EnvTask(ta2), EnvTask(ta3)) =>
        EnvTask(Task.parMap3(ta1, ta2, ta3)(f))
      case _                                          =>
        Env(ctx => Task.parMap3(fa1.run(ctx), fa2.run(ctx), fa3.run(ctx))(f))
    }

  def parMap4[E, A1, A2, A3, A4, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3], fa4: Env[E, A4])(
      f: (A1, A2, A3, A4) => R
  ): Env[E, R] =
    (fa1, fa2, fa3, fa4) match {
      case (EnvTask(ta1), EnvTask(ta2), EnvTask(ta3), EnvTask(ta4)) =>
        EnvTask(Task.parMap4(ta1, ta2, ta3, ta4)(f))
      case _                                                        =>
        Env(ctx => Task.parMap4(fa1.run(ctx), fa2.run(ctx), fa3.run(ctx), fa4.run(ctx))(f))
    }

  def parMap5[E, A1, A2, A3, A4, A5, R](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5]
  )(f: (A1, A2, A3, A4, A5) => R): Env[E, R] =
    (fa1, fa2, fa3, fa4, fa5) match {
      case (EnvTask(ta1), EnvTask(ta2), EnvTask(ta3), EnvTask(ta4), EnvTask(ta5)) =>
        EnvTask(Task.parMap5(ta1, ta2, ta3, ta4, ta5)(f))
      case _                                                                      =>
        Env(ctx => Task.parMap5(fa1.run(ctx), fa2.run(ctx), fa3.run(ctx), fa4.run(ctx), fa5.run(ctx))(f))
    }

  def parMap6[E, A1, A2, A3, A4, A5, A6, R](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5],
      fa6: Env[E, A6]
  )(f: (A1, A2, A3, A4, A5, A6) => R): Env[E, R] =
    (fa1, fa2, fa3, fa4, fa5, fa6) match {
      case (EnvTask(ta1), EnvTask(ta2), EnvTask(ta3), EnvTask(ta4), EnvTask(ta5), EnvTask(ta6)) =>
        EnvTask(Task.parMap6(ta1, ta2, ta3, ta4, ta5, ta6)(f))
      case _                                                                                    =>
        Env(ctx => Task.parMap6(fa1.run(ctx), fa2.run(ctx), fa3.run(ctx), fa4.run(ctx), fa5.run(ctx), fa6.run(ctx))(f))
    }

  def map2[E, A1, A2, R](fa1: Env[E, A1], fa2: Env[E, A2])(f: (A1, A2) => R): Env[E, R] =
    (fa1, fa2) match {
      case (EnvTask(ta1), EnvTask(ta2)) => EnvTask(Task.map2(ta1, ta2)(f))
      case _                            => Env(ctx => Task.map2(fa1.run(ctx), fa2.run(ctx))(f))
    }

  def map3[E, A1, A2, A3, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3])(f: (A1, A2, A3) => R): Env[E, R] =
    (fa1, fa2, fa3) match {
      case (EnvTask(ta1), EnvTask(ta2), EnvTask(ta3)) =>
        EnvTask(Task.map3(ta1, ta2, ta3)(f))
      case _                                          =>
        Env(ctx => Task.map3(fa1.run(ctx), fa2.run(ctx), fa3.run(ctx))(f))
    }

  def map4[E, A1, A2, A3, A4, R](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3], fa4: Env[E, A4])(
      f: (A1, A2, A3, A4) => R
  ): Env[E, R] =
    (fa1, fa2, fa3, fa4) match {
      case (EnvTask(ta1), EnvTask(ta2), EnvTask(ta3), EnvTask(ta4)) =>
        EnvTask(Task.map4(ta1, ta2, ta3, ta4)(f))
      case _                                                        =>
        Env(ctx => Task.map4(fa1.run(ctx), fa2.run(ctx), fa3.run(ctx), fa4.run(ctx))(f))
    }

  def map5[E, A1, A2, A3, A4, A5, R](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5]
  )(f: (A1, A2, A3, A4, A5) => R): Env[E, R] =
    (fa1, fa2, fa3, fa4, fa5) match {
      case (EnvTask(ta1), EnvTask(ta2), EnvTask(ta3), EnvTask(ta4), EnvTask(ta5)) =>
        EnvTask(Task.map5(ta1, ta2, ta3, ta4, ta5)(f))
      case _                                                                      =>
        Env(ctx => Task.map5(fa1.run(ctx), fa2.run(ctx), fa3.run(ctx), fa4.run(ctx), fa5.run(ctx))(f))
    }

  def map6[E, A1, A2, A3, A4, A5, A6, R](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5],
      fa6: Env[E, A6]
  )(f: (A1, A2, A3, A4, A5, A6) => R): Env[E, R] =
    (fa1, fa2, fa3, fa4, fa5, fa6) match {
      case (EnvTask(ta1), EnvTask(ta2), EnvTask(ta3), EnvTask(ta4), EnvTask(ta5), EnvTask(ta6)) =>
        EnvTask(Task.map6(ta1, ta2, ta3, ta4, ta5, ta6)(f))
      case _                                                                                    =>
        Env(ctx => Task.map6(fa1.run(ctx), fa2.run(ctx), fa3.run(ctx), fa4.run(ctx), fa5.run(ctx), fa6.run(ctx))(f))
    }

  def parZip2[E, A1, A2](fa1: Env[E, A1], fa2: Env[E, A2]): Env[E, (A1, A2)] =
    fa1.mapTask2(fa2)(Task.parZip2)

  def parZip3[E, A1, A2, A3](fa1: Env[E, A1], fa2: Env[E, A2], fa3: Env[E, A3]): Env[E, (A1, A2, A3)] =
    fa1.mapTask3(fa2, fa3)(Task.parZip3)

  def parZip4[E, A1, A2, A3, A4](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4]
  ): Env[E, (A1, A2, A3, A4)] =
    fa1.mapTask4(fa2, fa3, fa4)(Task.parZip4)

  def parZip5[E, A1, A2, A3, A4, A5](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5]
  ): Env[E, (A1, A2, A3, A4, A5)] =
    fa1.mapTask5(fa2, fa3, fa4, fa5)(Task.parZip5)

  def parZip6[E, A1, A2, A3, A4, A5, A6](
      fa1: Env[E, A1],
      fa2: Env[E, A2],
      fa3: Env[E, A3],
      fa4: Env[E, A4],
      fa5: Env[E, A5],
      fa6: Env[E, A6]
  ): Env[E, (A1, A2, A3, A4, A5, A6)] =
    fa1.mapTask6(fa2, fa3, fa4, fa5, fa6)(Task.parZip6)
}

private[env] trait EnvTransformations {
  self: Env.type =>
  final def taskNat[E]: Task ~> Env[E, *]                   = new FunctionK[Task, Env[E, *]] {
    override def apply[A](fa: Task[A]): Env[E, A] = Env.fromTask(fa)
  }
  final def funcNat[E]: (E => *) ~> Env[E, *]               = new FunctionK[E => *, Env[E, *]] {
    override def apply[A](fa: E => A): Env[E, A] = Env.fromFunc(fa)
  }
  final def idNat[E]: Id ~> Env[E, *]                       = new FunctionK[Id, Env[E, *]] {
    override def apply[A](fa: Id[A]): Env[E, A] = Env.pure(fa)
  }
  final def tryNat[E]: Try ~> Env[E, *]                     = new FunctionK[Try, Env[E, *]] {
    override def apply[A](fa: Try[A]): Env[E, A] = Env.fromTry(fa)
  }
  final def eitherNat[E]: Either[Throwable, *] ~> Env[E, *] = new FunctionK[Either[Throwable, *], Env[E, *]] {
    override def apply[A](fa: Either[Throwable, A]): Env[E, A] = Env.fromEither(fa)
  }
  final def coevalNat[E]: Coeval ~> Env[E, *]               = new FunctionK[Coeval, Env[E, *]] {
    override def apply[A](fa: Coeval[A]): Env[E, A] = Env.fromCoeval(fa)
  }
}
