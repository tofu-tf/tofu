package tofu.optics.tags

trait PTagApply[+O[_, _, _, _], A, B, U, V, Tag, X] {
  def continue(x: X): O[A, B, U, V]
}

trait TagApply[+O[_, _, _, _], A, U, Tag, X] extends PTagApply[O, A, A, U, U, Tag, X]

trait Tagger[+O[_, _, _, _]] {
  type Tag
}

trait TaggerObj[+O[_, _, _, _]] extends Tagger[O] {
  type Tag = this.type
}
