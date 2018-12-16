package cats
package tests

import catalysts.Platform

import cats.instances.{
  AllInstances,
  AllInstancesBinCompat0,
  AllInstancesBinCompat1,
  AllInstancesBinCompat2,
  AllInstancesBinCompat3
}
import cats.syntax.{
  AllSyntax,
  AllSyntaxBinCompat0,
  AllSyntaxBinCompat1,
  AllSyntaxBinCompat2,
  AllSyntaxBinCompat3,
  EqOps
}
import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.scalatest.{FunSuite, FunSuiteLike, Matchers}
import org.scalatest.prop.{Configuration, GeneratorDrivenPropertyChecks}
import org.typelevel.discipline.scalatest.Discipline

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
    extends FunSuite
    with Matchers
    with GeneratorDrivenPropertyChecks
    with Discipline
    with TestSettings
    with AllInstances
    with AllInstancesBinCompat0
    with AllInstancesBinCompat1
    with AllInstancesBinCompat2
    with AllInstancesBinCompat3
    with AllSyntax
    with AllSyntaxBinCompat0
    with AllSyntaxBinCompat1
    with AllSyntaxBinCompat2
    with AllSyntaxBinCompat3
    with StrictCatsEquality { self: FunSuiteLike =>

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
