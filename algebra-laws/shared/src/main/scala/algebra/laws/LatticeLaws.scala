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

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.*
import scala.annotation.nowarn

object LatticeLaws {
  def apply[A: Eq: Arbitrary] = new LatticeLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

@nowarn("msg=deprecated")
trait LatticeLaws[A] extends GroupLaws[A] {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

  def joinSemilattice(implicit A: JoinSemilattice[A]) = new LatticeProperties(
    name = "joinSemilattice",
    parents = Nil,
    join = Some(semilattice(A.joinSemilattice)),
    meet = None,
    Rules.serializable(A)
  )

  def meetSemilattice(implicit A: MeetSemilattice[A]) = new LatticeProperties(
    name = "meetSemilattice",
    parents = Nil,
    join = None,
    meet = Some(semilattice(A.meetSemilattice)),
    Rules.serializable(A)
  )

  def lattice(implicit A: Lattice[A]) = new LatticeProperties(
    name = "lattice",
    parents = Seq(joinSemilattice, meetSemilattice),
    join = Some(semilattice(A.joinSemilattice)),
    meet = Some(semilattice(A.meetSemilattice)),
    "absorption" -> forAll { (x: A, y: A) =>
      (A.join(x, A.meet(x, y)) ?== x) && (A.meet(x, A.join(x, y)) ?== x)
    }
  )

  def distributiveLattice(implicit A: DistributiveLattice[A]) = new LatticeProperties(
    name = "distributiveLattice",
    parents = Seq(lattice),
    join = Some(semilattice(A.joinSemilattice)),
    meet = Some(semilattice(A.meetSemilattice)),
    "distributive" -> forAll { (x: A, y: A, z: A) =>
      (A.join(x, A.meet(y, z)) ?== A.meet(A.join(x, y), A.join(x, z))) &&
      (A.meet(x, A.join(y, z)) ?== A.join(A.meet(x, y), A.meet(x, z)))
    }
  )

  def boundedJoinSemilattice(implicit A: BoundedJoinSemilattice[A]) = new LatticeProperties(
    name = "boundedJoinSemilattice",
    parents = Seq(joinSemilattice),
    join = Some(boundedSemilattice(A.joinSemilattice)),
    meet = None
  )

  def boundedMeetSemilattice(implicit A: BoundedMeetSemilattice[A]) = new LatticeProperties(
    name = "boundedMeetSemilattice",
    parents = Seq(meetSemilattice),
    join = None,
    meet = Some(boundedSemilattice(A.meetSemilattice))
  )

  def boundedJoinLattice(implicit A: Lattice[A] with BoundedJoinSemilattice[A]) = new LatticeProperties(
    name = "boundedJoinLattice",
    parents = Seq(boundedJoinSemilattice, lattice),
    join = Some(boundedSemilattice(A.joinSemilattice)),
    meet = Some(semilattice(A.meetSemilattice))
  )

  def boundedMeetLattice(implicit A: Lattice[A] with BoundedMeetSemilattice[A]) = new LatticeProperties(
    name = "boundedMeetLattice",
    parents = Seq(boundedMeetSemilattice, lattice),
    join = Some(semilattice(A.joinSemilattice)),
    meet = Some(boundedSemilattice(A.meetSemilattice))
  )

  def boundedLattice(implicit A: BoundedLattice[A]) = new LatticeProperties(
    name = "boundedLattice",
    parents = Seq(boundedJoinSemilattice, boundedMeetSemilattice, lattice),
    join = Some(boundedSemilattice(A.joinSemilattice)),
    meet = Some(boundedSemilattice(A.meetSemilattice))
  )

  def boundedDistributiveLattice(implicit A: BoundedDistributiveLattice[A]) = new LatticeProperties(
    name = "boundedLattice",
    parents = Seq(boundedLattice, distributiveLattice),
    join = Some(boundedSemilattice(A.joinSemilattice)),
    meet = Some(boundedSemilattice(A.meetSemilattice))
  )

  class LatticeProperties(
    val name: String,
    val parents: Seq[LatticeProperties],
    val join: Option[GroupProperties],
    val meet: Option[GroupProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    private val _m = meet.map { "meet" -> _ }
    private val _j = join.map { "join" -> _ }

    val bases = _m.toList ::: _j.toList
  }

}
