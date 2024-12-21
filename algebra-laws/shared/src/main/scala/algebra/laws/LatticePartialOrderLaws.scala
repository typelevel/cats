/*
 * Copyright (c) 2015 Typelevel
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

import algebra.*
import algebra.lattice.*

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.*

import algebra.instances.boolean.*

object LatticePartialOrderLaws {
  def apply[A: Eq: Arbitrary] = new LatticePartialOrderLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait LatticePartialOrderLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def joinSemilatticePartialOrder(implicit A: JoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "joinSemilatticePartialOrder",
      parents = Seq.empty,
      "join+lteqv" -> forAll { (x: A, y: A) =>
        P.lteqv(x, y) ?== P.eqv(y, A.join(x, y))
      }
    )

  def meetSemilatticePartialOrder(implicit A: MeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "meetSemilatticePartialOrder",
      parents = Seq.empty,
      "meet+lteqv" -> forAll { (x: A, y: A) =>
        P.lteqv(x, y) ?== P.eqv(x, A.meet(x, y))
      }
    )

  def latticePartialOrder(implicit A: Lattice[A], P: PartialOrder[A]) = new LatticePartialOrderProperties(
    name = "latticePartialOrder",
    parents = Seq(joinSemilatticePartialOrder, meetSemilatticePartialOrder)
  )

  def boundedJoinSemilatticePartialOrder(implicit A: BoundedJoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedJoinSemilatticePartialOrder",
      parents = Seq(joinSemilatticePartialOrder),
      "lteqv+zero" -> forAll { (x: A) => A.zero ?<= x }
    )

  def boundedMeetSemilatticePartialOrder(implicit A: BoundedMeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedMeetSemilatticePartialOrder",
      parents = Seq(meetSemilatticePartialOrder),
      "lteqv+one" -> forAll { (x: A) => x ?<= A.one }
    )

  def boundedBelowLatticePartialOrder(implicit A: Lattice[A] with BoundedJoinSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedBelowLatticePartialOrder",
      parents = Seq(boundedJoinSemilatticePartialOrder, latticePartialOrder)
    )

  def boundedAboveLatticePartialOrder(implicit A: Lattice[A] with BoundedMeetSemilattice[A], P: PartialOrder[A]) =
    new LatticePartialOrderProperties(
      name = "boundedAboveLatticePartialOrder",
      parents = Seq(boundedMeetSemilatticePartialOrder, latticePartialOrder)
    )

  def boundedLatticePartialOrder(implicit A: BoundedLattice[A], P: PartialOrder[A]) = new LatticePartialOrderProperties(
    name = "boundedLatticePartialOrder",
    parents = Seq(boundedJoinSemilatticePartialOrder, boundedMeetSemilatticePartialOrder)
  )

  class LatticePartialOrderProperties(
    val name: String,
    val parents: Seq[LatticePartialOrderProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    def bases = Nil
  }

}
