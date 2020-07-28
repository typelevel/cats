package cats.tests

import cats.platform.Platform
import org.scalacheck.{Test => ScalaCheckTest}

trait TestSettings {

  lazy val checkConfiguration: ScalaCheckTest.Parameters =
    ScalaCheckTest.Parameters.default
      .withMinSuccessfulTests(if (Platform.isJvm) 50 else 5)
      .withMaxDiscardRatio(if (Platform.isJvm) 5.0f else 50.0f)
      .withMaxSize(if (Platform.isJvm) 10 else 5)
      .withMinSize(0)
      .withWorkers(if (Platform.isJvm) 2 else 1)

  lazy val slowCheckConfiguration: ScalaCheckTest.Parameters =
    if (Platform.isJvm) checkConfiguration
    else
      ScalaCheckTest.Parameters.default
        .withMinSuccessfulTests(1)
        .withMaxSize(ScalaCheckTest.Parameters.default.minSize + 1)
}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite extends munit.DisciplineSuite with TestSettings {

  implicit override def scalaCheckTestParameters: ScalaCheckTest.Parameters =
    checkConfiguration

  def even(i: Int): Boolean = i % 2 == 0

  val evenPf: PartialFunction[Int, Int] = { case i if even(i) => i }
}

trait SlowCatsSuite extends CatsSuite {
  implicit override def scalaCheckTestParameters: ScalaCheckTest.Parameters =
    slowCheckConfiguration
}
