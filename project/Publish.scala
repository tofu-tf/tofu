import sbt.SettingKey
object Publish {
  val publishVersion: SettingKey[String] = SettingKey(
    label = "publishVersion",
    description = "version prefix, it will be *the* version of module if branch is master"
  )

  val publishName: SettingKey[String] = SettingKey(
    label = "publishName",
    description = "module name, it will be prefixed with tofu- in the artifact name"
  )
}
