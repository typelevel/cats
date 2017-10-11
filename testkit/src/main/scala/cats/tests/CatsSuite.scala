package cats
package tests

import catalysts.Platform

import cats.instances.AllInstances
import cats.syntax.{AllSyntax, EqOps}

import org.scalactic.anyvals.{PosZDouble, PosInt, PosZInt}
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
      workers = if (Platform.isJvm) PosInt(2) else PosInt(1))

  lazy val slowCheckConfiguration: PropertyCheckConfiguration =
    if (Platform.isJvm) checkConfiguration
    else PropertyCheckConfiguration(minSuccessful = 1, sizeRange = 1)
}

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite extends FunSuite
    with Matchers
    with GeneratorDrivenPropertyChecks
    with Discipline
    with TestSettings
    with AllInstances
    with AllSyntax
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
