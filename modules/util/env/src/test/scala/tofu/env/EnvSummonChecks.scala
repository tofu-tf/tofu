package tofu.env
import tofu._

object EnvSummonChecks {
  implicitly[Fire[Env[Unit, *]]]
  implicitly[Start[Env[Unit, *]]]
  implicitly[Race[Env[Unit, *]]]
}
