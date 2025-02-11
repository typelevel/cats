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

package algebra
package laws

import algebra.ring.*

import algebra.laws.platform.Platform

import org.typelevel.discipline.Predicate

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*
import scala.annotation.nowarn

object RingLaws {
  def apply[A: Eq: Arbitrary: AdditiveMonoid]: RingLaws[A] =
    withPred[A](new Predicate[A] {
      def apply(a: A): Boolean = Eq[A].neqv(a, AdditiveMonoid[A].zero)
    })

  def withPred[A](pred0: Predicate[A])(implicit eqv: Eq[A], arb: Arbitrary[A]): RingLaws[A] = new RingLaws[A] {
    def Arb = arb
    def pred = pred0
    @nowarn("msg=deprecated")
    val nonZeroLaws = new GroupLaws[A] {
      def Arb = Arbitrary(arbitrary[A](arb).filter(pred))
      def Equ = eqv
    }
  }
}

@nowarn("msg=deprecated")
trait RingLaws[A] extends GroupLaws[A] { self =>

  // must be a val (stable identifier)
  val nonZeroLaws: GroupLaws[A]
  def pred: Predicate[A]

  def withPred(pred0: Predicate[A], replace: Boolean = true): RingLaws[A] =
    RingLaws.withPred(if (replace) pred0 else pred && pred0)(Equ, Arb)

  def setNonZeroParents(props: nonZeroLaws.GroupProperties,
                        parents: Seq[nonZeroLaws.GroupProperties]
  ): nonZeroLaws.GroupProperties =
    new nonZeroLaws.GroupProperties(
      name = props.name,
      parents = parents,
      props = props.props: _*
    )

  implicit def Arb: Arbitrary[A]
  implicit def Equ: Eq[A] = nonZeroLaws.Equ

  // additive groups

  def additiveSemigroup(implicit A: AdditiveSemigroup[A]) = new AdditiveProperties(
    base = semigroup(A.additive),
    parents = Nil,
    Rules.serializable(A),
    Rules.repeat1("sumN")(A.sumN),
    Rules.repeat2("sumN", "+")(A.sumN)(A.plus)
  )

  def additiveCommutativeSemigroup(implicit A: AdditiveCommutativeSemigroup[A]) = new AdditiveProperties(
    base = commutativeSemigroup(A.additive),
    parents = List(additiveSemigroup)
  )

  def additiveMonoid(implicit A: AdditiveMonoid[A]) = new AdditiveProperties(
    base = monoid(A.additive),
    parents = List(additiveSemigroup),
    Rules.repeat0("sumN", "zero", A.zero)(A.sumN),
    Rules.collect0("sum", "zero", A.zero)(A.sum)
  )

  def additiveCommutativeMonoid(implicit A: AdditiveCommutativeMonoid[A]) = new AdditiveProperties(
    base = commutativeMonoid(A.additive),
    parents = List(additiveMonoid)
  )

  def additiveGroup(implicit A: AdditiveGroup[A]) = new AdditiveProperties(
    base = group(A.additive),
    parents = List(additiveMonoid),
    Rules.consistentInverse("subtract")(A.minus)(A.plus)(A.negate)
  )

  def additiveCommutativeGroup(implicit A: AdditiveCommutativeGroup[A]) = new AdditiveProperties(
    base = commutativeGroup(A.additive),
    parents = List(additiveGroup)
  )

  // multiplicative groups

  def multiplicativeSemigroup(implicit A: MultiplicativeSemigroup[A]) = new MultiplicativeProperties(
    base = semigroup(A.multiplicative),
    nonZeroBase = None,
    parent = None,
    Rules.serializable(A),
    Rules.repeat1("pow")(A.pow),
    Rules.repeat2("pow", "*")(A.pow)(A.times)
  )

  def multiplicativeCommutativeSemigroup(implicit A: MultiplicativeCommutativeSemigroup[A]) =
    new MultiplicativeProperties(
      base = semigroup(A.multiplicative),
      nonZeroBase = None,
      parent = Some(multiplicativeSemigroup)
    )

  def multiplicativeMonoid(implicit A: MultiplicativeMonoid[A]) = new MultiplicativeProperties(
    base = monoid(A.multiplicative),
    nonZeroBase = None,
    parent = Some(multiplicativeSemigroup),
    Rules.repeat0("pow", "one", A.one)(A.pow),
    Rules.collect0("product", "one", A.one)(A.product)
  )

  def multiplicativeCommutativeMonoid(implicit A: MultiplicativeCommutativeMonoid[A]) = new MultiplicativeProperties(
    base = commutativeMonoid(A.multiplicative),
    nonZeroBase = None,
    parent = Some(multiplicativeMonoid)
  )

  def multiplicativeGroup(implicit A: MultiplicativeGroup[A]) = new MultiplicativeProperties(
    base = monoid(A.multiplicative),
    nonZeroBase = Some(setNonZeroParents(nonZeroLaws.group(A.multiplicative), Nil)),
    parent = Some(multiplicativeMonoid),
    // pred is used to ensure y is not zero.
    "consistent division" -> forAll { (x: A, y: A) =>
      pred(y) ==> (A.div(x, y) ?== A.times(x, A.reciprocal(y)))
    }
  )

  def multiplicativeCommutativeGroup(implicit A: MultiplicativeCommutativeGroup[A]) = new MultiplicativeProperties(
    base = commutativeMonoid(A.multiplicative),
    nonZeroBase =
      Some(setNonZeroParents(nonZeroLaws.commutativeGroup(A.multiplicative), multiplicativeGroup.nonZeroBase.toSeq)),
    parent = Some(multiplicativeGroup)
  )

  // rings

  def semiring(implicit A: Semiring[A]) = new RingProperties(
    name = "semiring",
    al = additiveCommutativeMonoid,
    ml = multiplicativeSemigroup,
    parents = Seq.empty,
    Rules.distributive(A.plus)(A.times)
  )

  def rng(implicit A: Rng[A]) = new RingProperties(
    name = "rng",
    al = additiveCommutativeGroup,
    ml = multiplicativeSemigroup,
    parents = Seq(semiring)
  )

  def rig(implicit A: Rig[A]) = new RingProperties(
    name = "rig",
    al = additiveCommutativeMonoid,
    ml = multiplicativeMonoid,
    parents = Seq(semiring)
  )

  def ring(implicit A: Ring[A]) = new RingProperties(
    // TODO fromParents
    name = "ring",
    al = additiveCommutativeGroup,
    ml = multiplicativeMonoid,
    parents = Seq(rig, rng),
    "fromInt" -> forAll { (n: Int) =>
      Ring.fromInt[A](n) ?== A.sumN(A.one, n)
    },
    "fromBigInt" -> forAll { (ns: List[Int]) =>
      val actual = Ring.fromBigInt[A](ns.map(BigInt(_)).foldLeft(BigInt(1))(_ * _))
      val expected = ns.map(A.fromInt).foldLeft(A.one)(A.times)
      actual ?== expected
    }
  )

  // commutative rings

  def commutativeSemiring(implicit A: CommutativeSemiring[A]) = new RingProperties(
    name = "commutativeSemiring",
    al = additiveCommutativeMonoid,
    ml = multiplicativeCommutativeSemigroup,
    parents = Seq(semiring)
  )

  def commutativeRng(implicit A: CommutativeRng[A]) = new RingProperties(
    name = "commutativeRng",
    al = additiveCommutativeMonoid,
    ml = multiplicativeCommutativeSemigroup,
    parents = Seq(rng, commutativeSemiring)
  )

  def commutativeRig(implicit A: CommutativeRig[A]) = new RingProperties(
    name = "commutativeRig",
    al = additiveCommutativeMonoid,
    ml = multiplicativeCommutativeMonoid,
    parents = Seq(rig, commutativeSemiring)
  )

  def commutativeRing(implicit A: CommutativeRing[A]) = new RingProperties(
    name = "commutative ring",
    al = additiveCommutativeGroup,
    ml = multiplicativeCommutativeMonoid,
    parents = Seq(ring, commutativeRig, commutativeRng)
  )

  def gcdRing(implicit A: GCDRing[A]) = RingProperties.fromParent(
    name = "gcd domain",
    parent = commutativeRing,
    "gcd/lcm" -> forAll { (x: A, y: A) =>
      val d = A.gcd(x, y)
      val m = A.lcm(x, y)
      A.times(x, y) ?== A.times(d, m)
    },
    "gcd is commutative" -> forAll { (x: A, y: A) =>
      A.gcd(x, y) ?== A.gcd(y, x)
    },
    "lcm is commutative" -> forAll { (x: A, y: A) =>
      A.lcm(x, y) ?== A.lcm(y, x)
    },
    "gcd(0, 0)" -> (A.gcd(A.zero, A.zero) ?== A.zero),
    "lcm(0, 0) === 0" -> (A.lcm(A.zero, A.zero) ?== A.zero),
    "lcm(x, 0) === 0" -> forAll { (x: A) => A.lcm(x, A.zero) ?== A.zero }
  )

  def euclideanRing(implicit A: EuclideanRing[A]) = RingProperties.fromParent(
    name = "euclidean ring",
    parent = gcdRing,
    "euclidean division rule" -> forAll { (x: A, y: A) =>
      pred(y) ==> {
        val (q, r) = A.equotmod(x, y)
        x ?== A.plus(A.times(y, q), r)
      }
    },
    "equot" -> forAll { (x: A, y: A) =>
      pred(y) ==> {
        A.equotmod(x, y)._1 ?== A.equot(x, y)
      }
    },
    "emod" -> forAll { (x: A, y: A) =>
      pred(y) ==> {
        A.equotmod(x, y)._2 ?== A.emod(x, y)
      }
    },
    "euclidean function" -> forAll { (x: A, y: A) =>
      pred(y) ==> {
        val (_, r) = A.equotmod(x, y)
        A.isZero(r) || (A.euclideanFunction(r) < A.euclideanFunction(y))
      }
    },
    "submultiplicative function" -> forAll { (x: A, y: A) =>
      (pred(x) && pred(y)) ==> {
        A.euclideanFunction(x) <= A.euclideanFunction(A.times(x, y))
      }
    }
  )

  def semifield(implicit A: Semifield[A]) = new RingProperties(
    name = "semifield",
    al = additiveCommutativeMonoid,
    ml = multiplicativeGroup,
    parents = Seq(rig)
  )

  def commutativeSemifield(implicit A: CommutativeSemifield[A]) = new RingProperties(
    name = "semifield",
    al = additiveCommutativeMonoid,
    ml = multiplicativeCommutativeGroup,
    parents = Seq(semifield, commutativeRig)
  )

  def divisionRing(implicit A: DivisionRing[A]) = new RingProperties(
    name = "division ring",
    al = additiveCommutativeGroup,
    ml = multiplicativeGroup,
    parents = Seq(ring, semifield),
    "fromDouble" -> forAll { (n: Double) =>
      if (Platform.isJvm) {
        // TODO: BigDecimal(n) is busted in scalajs, so we skip this test.
        val bd = new java.math.BigDecimal(n)
        val unscaledValue = new BigInt(bd.unscaledValue)
        val expected =
          if (bd.scale > 0) {
            A.div(A.fromBigInt(unscaledValue), A.fromBigInt(BigInt(10).pow(bd.scale)))
          } else {
            A.fromBigInt(unscaledValue * BigInt(10).pow(-bd.scale))
          }
        DivisionRing.fromDouble[A](n) ?== expected
      } else {
        Prop(true)
      }
    }
  )

  // boolean rings

  def boolRng(implicit A: BoolRng[A]) = RingProperties.fromParent(
    name = "boolean rng",
    parent = commutativeRng,
    Rules.idempotence(A.times)
  )

  def boolRing(implicit A: BoolRing[A]) = RingProperties.fromParent(
    name = "boolean ring",
    parent = commutativeRing,
    Rules.idempotence(A.times)
  )

  // Everything below fields (e.g. rings) does not require their multiplication
  // operation to be a group. Hence, we do not check for the existence of an
  // inverse. On the other hand, fields require their multiplication to be an
  // abelian group. Now we have to worry about zero.
  //
  // The usual text book definition says: Fields consist of two abelian groups
  // (set, +, zero) and (set \ zero, *, one). We do the same thing here.
  // However, since law checking for the multiplication does not include zero
  // any more, it is not immediately clear that desired properties like
  // zero * x == x * zero hold.
  // Luckily, these follow from the other field and group axioms.
  def field(implicit A: Field[A]) = new RingProperties(
    name = "field",
    al = additiveCommutativeGroup,
    ml = multiplicativeCommutativeGroup,
    parents = Seq(euclideanRing, divisionRing, commutativeSemifield)
  )

  // Approximate fields such a Float or Double, even through filtered using FPFilter, do not work well with
  // Euclidean ring tests
  def approxField(implicit A: Field[A]) = new RingProperties(
    name = "field",
    al = additiveCommutativeGroup,
    ml = multiplicativeCommutativeGroup,
    parents = Seq(commutativeRing),
    "fromDouble" -> forAll { (n: Double) =>
      if (Platform.isJvm) {
        // TODO: BigDecimal(n) is busted in scalajs, so we skip this test.
        val bd = new java.math.BigDecimal(n)
        val unscaledValue = new BigInt(bd.unscaledValue)
        val expected =
          if (bd.scale > 0) {
            A.div(A.fromBigInt(unscaledValue), A.fromBigInt(BigInt(10).pow(bd.scale)))
          } else {
            A.fromBigInt(unscaledValue * BigInt(10).pow(-bd.scale))
          }
        Field.fromDouble[A](n) ?== expected
      } else {
        Prop(true)
      }
    }
  )

  // property classes

  class AdditiveProperties(
    val base: GroupLaws[A]#GroupProperties,
    val parents: Seq[AdditiveProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    val name = "additive " + base.name
    val bases = List("base" -> base)
  }

  class MultiplicativeProperties(
    val base: GroupLaws[A]#GroupProperties,
    val nonZeroBase: Option[nonZeroLaws.GroupProperties],
    val parent: Option[MultiplicativeProperties],
    val props: (String, Prop)*
  ) extends RuleSet
      with HasOneParent {
    val name = "multiplicative " + base.name
    val bases = Seq("base" -> base) ++ nonZeroBase.map("non-zero base" -> _)
  }

  object RingProperties {
    def fromParent(name: String, parent: RingProperties, props: (String, Prop)*) =
      new RingProperties(name, parent.al, parent.ml, Seq(parent), props: _*)
  }

  class RingProperties(
    val name: String,
    val al: AdditiveProperties,
    val ml: MultiplicativeProperties,
    val parents: Seq[RingProperties],
    val props: (String, Prop)*
  ) extends RuleSet {
    def bases = Seq("additive" -> al, "multiplicative" -> ml)
  }
}
