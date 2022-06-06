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

import algebra._
import algebra.lattice._
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._
import org.typelevel.discipline.Laws

@deprecated("Laws moved to LogicLaws", since = "2.7.0")
object DeMorganLaws {
  def apply[A: Eq: Arbitrary: LatticeLaws] = new DeMorganLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
    def LL = implicitly[LatticeLaws[A]]
  }
}

@deprecated("Laws moved to LogicLaws", since = "2.7.0")
trait DeMorganLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]
  def LL: LatticeLaws[A]

  def logic(implicit A: Logic[A]) = new DeMorganProperties(
    name = "logic",
    parents = Seq(),
    ll = LL.boundedDistributiveLattice,
    Rules.distributive(A.or)(A.and),
    "¬(x∨y) = ¬x∧¬y" -> forAll { (x: A, y: A) => A.not(A.or(x, y)) ?== A.and(A.not(x), A.not(y)) },
    "¬(x∧y) = ¬¬(¬x∨¬y)" -> forAll { (x: A, y: A) => A.not(A.and(x, y)) ?== A.not(A.not(A.or(A.not(x), A.not(y)))) }
  )

  def deMorgan(implicit A: DeMorgan[A]) = new DeMorganProperties(
    name = "deMorgan",
    parents = Seq(logic),
    ll = LL.boundedDistributiveLattice,
    Rules.distributive(A.or)(A.and),
    "involutive" -> forAll { (x: A) => A.not(A.not(x)) ?== x }
  )

  class DeMorganProperties(
    val name: String,
    val parents: Seq[DeMorganProperties],
    val ll: LatticeLaws[A]#LatticeProperties,
    val props: (String, Prop)*
  ) extends RuleSet {
    val bases = Seq("lattice" -> ll)
  }

}
