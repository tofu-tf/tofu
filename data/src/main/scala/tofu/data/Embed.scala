package tofu.data

object Embed{
  trait EmbedTag extends Any

  type T[+F[+_], +G[+_], +A] <: EmbedTag

  def apply[F[+_], G[+_], A](tfa: F[G[A]]): T[F, G, A] = tfa.asInstanceOf[T[F, G, A]]

  implicit class EmbedOps[F[+_], G[+_], A](private val e: T[F, G, A]) extends AnyVal{
    def value: F[G[A]] = e.asInstanceOf[F[G[A]]]
  }
}