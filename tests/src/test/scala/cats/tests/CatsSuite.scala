package cats
package tests

import cats.std.AllInstances
import cats.syntax.AllSyntax
import org.scalatest.FunSuite
import org.typelevel.discipline.scalatest.Discipline

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Cats tests.
 */
trait CatsSuite extends FunSuite with Discipline with AllInstances with AllSyntax
