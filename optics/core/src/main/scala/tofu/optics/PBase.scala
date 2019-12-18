package tofu.optics

trait PBase[-S, +T, +A, -B] {
  def label[label]: this.type with Label[label] = this.asInstanceOf[this.type with Label[label]]
}
