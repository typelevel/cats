package cats
package tests

import cats.instances._
import cats.platform.Platform
import cats.syntax._
import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

trait TestSettings extends Configuration with Matchers {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = if (Platform.isJvm) PosInt(50) else PosInt(5),
      maxDiscardedFactor = if (Platform.isJvm) PosZDouble(5.0) else PosZDouble(50.0),
      minSize = PosZInt(0),
      sizeRange = if (Platform.isJvm) PosZInt(10) else PosZInt(5),
      workers = if (Platform.isJvm) PosInt(2) else PosInt(1)
    )

  lazy val slowCheckConfiguration: PropertyCheckConfiguration =
    if (Platform.isJvm) checkConfiguration
    else PropertyCheckConfiguration(minSuccessful = 1, sizeRange = 1)
}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite
    extends AnyFunSuiteLike
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with FunSuiteDiscipline
    with TestSettings
    with AllInstances
    with AllInstancesBinCompat0
    with AllInstancesBinCompat1
    with AllInstancesBinCompat2
    with AllInstancesBinCompat3
    with AllInstancesBinCompat4
    with AllInstancesBinCompat5
    with AllInstancesBinCompat6
    with AllSyntax
    with AllSyntaxBinCompat0
    with AllSyntaxBinCompat1
    with AllSyntaxBinCompat2
    with AllSyntaxBinCompat3
    with AllSyntaxBinCompat4
    with AllSyntaxBinCompat5
    with AllSyntaxBinCompat6
    with AllSyntaxBinCompat7
    with StrictCatsEquality {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  // disable Eq syntax (by making `catsSyntaxEq` not implicit), since it collides
  // with scalactic's equality
  override def catsSyntaxEq[A: Eq](a: A): EqOps[A] = new EqOps[A](a)

  def even(i: Int): Boolean = i % 2 == 0

  val evenPf: PartialFunction[Int, Int] = { case i if even(i) => i }
}

trait SlowCatsSuite extends CatsSuite {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    slowCheckConfiguration
}
