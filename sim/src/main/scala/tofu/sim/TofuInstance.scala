package tofu.sim
import cats.effect.Fiber
import tofu.Start
import tofu.sim.SIM.RUN
//
//class TofuInstance[F[_, _]] extends Start[F[RUN, *]]{
//  def start[A](fa: F[RUN, A]): F[RUN, Fiber[F[RUN, *], A]] = ???
//  def racePair[A, B](fa: F[RUN, A], fb: F[RUN, B]): F[RUN, Either[(A, Fiber[F[RUN, *], B]), (Fiber[F[RUN, *], A], B)]] = ???
//  def race[A, B](fa: F[RUN, A], fb: F[RUN, B]): F[RUN, Either[A, B]] = ???
//  def never[A]: F[RUN, A] = ???
//  def fireAndForget[A](fa: F[RUN, A]): F[RUN, Unit] = ???
//}
