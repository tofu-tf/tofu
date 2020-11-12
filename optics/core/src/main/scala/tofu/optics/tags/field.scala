package tofu.optics.tags
import tofu.optics.PContains

object field extends TaggerObj[PContains] {
  implicit def field1T2[A, B, A1]: PTagApply[PContains, (A, B), (A1, B), A, A1, this.type, first.type] =
    _ => PContains[(A, B), A1](_._1)((t, x) => t.copy(_1 = x))

  implicit def field1T3[A, B, C, A1]: PTagApply[PContains, (A, B, C), (A1, B, C), A, A1, this.type, first.type] =
    _ => PContains[(A, B, C), A1](_._1)((t, x) => t.copy(_1 = x))

  implicit def field1T4[A, B, C, D, A1]
      : PTagApply[PContains, (A, B, C, D), (A1, B, C, D), A, A1, this.type, first.type] =
    _ => PContains[(A, B, C, D), A1](_._1)((t, x) => t.copy(_1 = x))

  implicit def field2T2[A, B, B1]: PTagApply[PContains, (A, B), (A, B1), B, B1, this.type, second.type] =
    _ => PContains[(A, B), B1](_._2)((t, x) => t.copy(_2 = x))

  implicit def field2T3[A, B, C, B1]: PTagApply[PContains, (A, B, C), (A, B1, C), B, B1, this.type, second.type] =
    _ => PContains[(A, B, C), B1](_._2)((t, x) => t.copy(_2 = x))

  implicit def field2T4[A, B, C, D, B1]
      : PTagApply[PContains, (A, B, C, D), (A, B1, C, D), B, B1, this.type, second.type] =
    _ => PContains[(A, B, C, D), B1](_._2)((t, x) => t.copy(_2 = x))

  implicit def field3T3[A, B, C, C1]: PTagApply[PContains, (A, B, C), (A, B, C1), C, C1, this.type, third.type] =
    _ => PContains[(A, B, C), C1](_._3)((t, x) => t.copy(_3 = x))

  implicit def field3T4[A, B, C, D, C1]
      : PTagApply[PContains, (A, B, C, D), (A, B, C1, D), C, C1, this.type, third.type] =
    _ => PContains[(A, B, C, D), C1](_._3)((t, x) => t.copy(_3 = x))

  implicit def field4T4[A, B, C, D, D1]
      : PTagApply[PContains, (A, B, C, D), (A, B, C, D1), D, D1, this.type, fourth.type] =
    _ => PContains[(A, B, C, D), D1](_._4)((t, x) => t.copy(_4 = x))
}

case object first
case object second
case object third
case object fourth
