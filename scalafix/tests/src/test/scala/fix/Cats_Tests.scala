package fix

import scala.meta._
import scalafix.testkit._

class Cats_Tests
  extends SemanticRuleSuite(
    Database.load(Classpath(AbsolutePath(BuildInfo.inputClassdirectory))),
    AbsolutePath(BuildInfo.inputSourceroot),
    Seq(AbsolutePath(BuildInfo.outputSourceroot))
  ) {
  runAllTests()
}
