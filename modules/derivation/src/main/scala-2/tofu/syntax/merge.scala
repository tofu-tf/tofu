package tofu.syntax

import tofu.data.derived.Merge

object merge {
  implicit class TofuMergeOps[A](private val a: A) extends AnyVal {
    def merge(b: A)(implicit merge: Merge[A]): A = merge.merge(a, b)
  }
}
