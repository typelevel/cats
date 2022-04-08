/*
 * Copyright (c) 2022 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package algebra.laws

import cats.kernel._
import cats.kernel.instances.option._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

@deprecated("Provided by cats.kernel.laws", since = "2.7.0")
object GroupLaws {
  def apply[A: Eq: Arbitrary]: GroupLaws[A] = new GroupLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

@deprecated("Provided by cats.kernel.laws", since = "2.7.0")
trait GroupLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  // groups

  def semigroup(implicit A: Semigroup[A]): GroupProperties = new GroupProperties(
    name = "semigroup",
    parents = Nil,
    Rules.serializable(A),
    Rules.associativity(A.combine),
    Rules.repeat1("combineN")(A.combineN),
    Rules.repeat2("combineN", "|+|")(A.combineN)(A.combine),
    "combineAllOption" -> forAll { (xs: Vector[A]) =>
      A.combineAllOption(xs) ?== xs.reduceOption(A.combine)
    }
  )

  def band(implicit A: Band[A]): GroupProperties = new GroupProperties(
    name = "band",
    parents = List(semigroup),
    Rules.idempotence(A.combine),
    "isIdempotent" -> Semigroup.isIdempotent[A]
  )

  def commutativeSemigroup(implicit A: CommutativeSemigroup[A]): GroupProperties = new GroupProperties(
    name = "commutative semigroup",
    parents = List(semigroup),
    Rules.commutative(A.combine)
  )

  def semilattice(implicit A: Semilattice[A]): GroupProperties = new GroupProperties(
    name = "semilattice",
    parents = List(band, commutativeSemigroup)
  )

  def monoid(implicit A: Monoid[A]): GroupProperties = new GroupProperties(
    name = "monoid",
    parents = List(semigroup),
    Rules.leftIdentity(A.empty)(A.combine),
    Rules.rightIdentity(A.empty)(A.combine),
    Rules.repeat0("combineN", "id", A.empty)(A.combineN),
    Rules.collect0("combineAll", "id", A.empty)(A.combineAll),
    Rules.isId("isEmpty", A.empty)(A.isEmpty),
    "combineAll" -> forAll { (xs: Vector[A]) =>
      A.combineAll(xs) ?== (A.empty +: xs).reduce(A.combine)
    }
  )

  def commutativeMonoid(implicit A: CommutativeMonoid[A]): GroupProperties = new GroupProperties(
    name = "commutative monoid",
    parents = List(monoid, commutativeSemigroup)
  )

  def boundedSemilattice(implicit A: BoundedSemilattice[A]): GroupProperties = new GroupProperties(
    name = "boundedSemilattice",
    parents = List(commutativeMonoid, semilattice)
  )

  def group(implicit A: Group[A]): GroupProperties = new GroupProperties(
    name = "group",
    parents = List(monoid),
    Rules.leftInverse(A.empty)(A.combine)(A.inverse),
    Rules.rightInverse(A.empty)(A.combine)(A.inverse),
    Rules.consistentInverse("remove")(A.remove)(A.combine)(A.inverse)
  )

  def commutativeGroup(implicit A: CommutativeGroup[A]): GroupProperties = new GroupProperties(
    name = "commutative group",
    parents = List(group, commutativeMonoid)
  )

  // property classes

  class GroupProperties(
    val name: String,
    val parents: Seq[GroupProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Nil
  }
}
