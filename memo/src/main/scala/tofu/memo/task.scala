package tofu.memo

import monix.eval.Task

object task {
  implicit val memoizeTask: Memoize[Task] = new Memoize[Task] {
    override def memoize[A](fa: Task[A]): Task[A] = fa.memoize
    /** should be redefined if F is at least ApplicativeError */
    override def memoizeOnSuccess[A](fa: Task[A]): Task[A] = fa.memoizeOnSuccess
  }
}
