package tofu.logging.zlogs

import zio.Scope
import zio.test._

object ZLogsTest extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Tofu ZLogging")(
      test("plain with context") {
        assertCompletesZIO
      }
    )

}
