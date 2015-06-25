package cats
package tests

import cats.std.AllInstances
import cats.syntax.AllSyntax
import org.scalatest.{ FunSuite, Matchers }
import org.typelevel.discipline.scalatest.Discipline

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite extends FunSuite with Matchers with Discipline with AllInstances with AllSyntax {
  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???
}
