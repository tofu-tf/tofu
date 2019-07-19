package tofu.memo

sealed trait CacheMethod
object CacheMethod {
  case object MVar extends CacheMethod
  case object Ref extends CacheMethod
}