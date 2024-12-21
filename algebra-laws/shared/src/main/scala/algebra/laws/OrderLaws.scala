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

import cats.kernel.*

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop.*

import cats.kernel.instances.all.*
import algebra.ring.Signed
import algebra.ring.CommutativeRing
import algebra.ring.TruncatedDivision
import algebra.ring.AdditiveCommutativeGroup
import algebra.ring.GCDRing

@deprecated("Provided by cats.kernel.laws", since = "2.7.0")
object OrderLaws {
  def apply[A: Eq: Arbitrary: Cogen]: OrderLaws[A] =
    new OrderLaws[A] {
      def Equ = Eq[A]
      def Arb = implicitly[Arbitrary[A]]
      def Cog = implicitly[Cogen[A]]
    }
}

@deprecated("Provided by cats.kernel.laws", since = "2.7.0")
trait OrderLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]
  implicit def Cog: Cogen[A]

  def eqv: OrderProperties = new OrderProperties(
    name = "eq",
    parent = None,
    Rules.serializable(Equ),
    "reflexitivity-eq" -> forAll { (x: A) =>
      x ?== x
    },
    "symmetry-eq" -> forAll { (x: A, y: A) =>
      Equ.eqv(x, y) ?== Equ.eqv(y, x)
    },
    "antisymmetry-eq" -> forAll { (x: A, y: A, f: A => A) =>
      !Equ.eqv(x, y) ?|| Equ.eqv(f(x), f(y))
    },
    "transitivity-eq" -> forAll { (x: A, y: A, z: A) =>
      !(Equ.eqv(x, y) && Equ.eqv(y, z)) ?|| Equ.eqv(x, z)
    }
  )

  def partialOrder(implicit A: PartialOrder[A]): OrderProperties = new OrderProperties(
    name = "partialOrder",
    parent = Some(eqv),
    Rules.serializable(A),
    "reflexitivity" -> forAll { (x: A) =>
      x ?<= x
    },
    "antisymmetry" -> forAll { (x: A, y: A) =>
      !(A.lteqv(x, y) && A.lteqv(y, x)) ?|| A.eqv(x, y)
    },
    "transitivity" -> forAll { (x: A, y: A, z: A) =>
      !(A.lteqv(x, y) && A.lteqv(y, z)) ?|| A.lteqv(x, z)
    },
    "gteqv" -> forAll { (x: A, y: A) =>
      A.lteqv(x, y) ?== A.gteqv(y, x)
    },
    "lt" -> forAll { (x: A, y: A) =>
      A.lt(x, y) ?== (A.lteqv(x, y) && A.neqv(x, y))
    },
    "gt" -> forAll { (x: A, y: A) =>
      A.lt(x, y) ?== A.gt(y, x)
    },
    "partialCompare" -> forAll { (x: A, y: A) =>
      val c = A.partialCompare(x, y)
      ((c < 0) ?== A.lt(x, y)) && ((c == 0) ?== A.eqv(x, y)) && ((c > 0) ?== A.gt(x, y))
    },
    "pmin" -> forAll { (x: A, y: A) =>
      val c = A.partialCompare(x, y)
      val m = A.pmin(x, y)
      if (c < 0) m ?== Some(x)
      else if (c == 0) (m ?== Some(x)) && (m ?== Some(y))
      else if (c > 0) m ?== Some(y)
      else m ?== None
    },
    "pmax" -> forAll { (x: A, y: A) =>
      val c = A.partialCompare(x, y)
      val m = A.pmax(x, y)
      if (c < 0) m ?== Some(y)
      else if (c == 0) (m ?== Some(x)) && (m ?== Some(y))
      else if (c > 0) m ?== Some(x)
      else m ?== None
    }
  )

  def order(implicit A: Order[A]): OrderProperties = new OrderProperties(
    name = "order",
    parent = Some(partialOrder),
    "totality" -> forAll { (x: A, y: A) =>
      A.lteqv(x, y) ?|| A.lteqv(y, x)
    },
    "compare" -> forAll { (x: A, y: A) =>
      val c = A.compare(x, y)
      ((c < 0) ?== A.lt(x, y)) && ((c == 0) ?== A.eqv(x, y)) && ((c > 0) ?== A.gt(x, y))
    },
    "min" -> forAll { (x: A, y: A) =>
      val c = A.compare(x, y)
      val m = A.min(x, y)
      if (c < 0) m ?== x
      else if (c == 0) (m ?== x) && (m ?== y)
      else m ?== y
    },
    "max" -> forAll { (x: A, y: A) =>
      val c = A.compare(x, y)
      val m = A.max(x, y)
      if (c < 0) m ?== y
      else if (c == 0) (m ?== x) && (m ?== y)
      else m ?== x
    }
  )

  def signed(implicit A: Signed[A]) = new OrderProperties(
    name = "signed",
    parent = Some(order(A.order)),
    "abs non-negative" -> forAll((x: A) => A.sign(A.abs(x)) != Signed.Negative),
    "signum returns -1/0/1" -> forAll((x: A) => A.signum(A.abs(x)) <= 1),
    "signum is sign.toInt" -> forAll((x: A) => A.signum(x) == A.sign(x).toInt),
    "ordered group" -> forAll { (x: A, y: A, z: A) =>
      A.order.lteqv(x, y) ==> A.order.lteqv(A.additiveCommutativeMonoid.plus(x, z),
                                            A.additiveCommutativeMonoid.plus(y, z)
      )
    },
    "triangle inequality" -> forAll { (x: A, y: A) =>
      A.order.lteqv(A.abs(A.additiveCommutativeMonoid.plus(x, y)), A.additiveCommutativeMonoid.plus(A.abs(x), A.abs(y)))
    }
  )

  def signedAdditiveCommutativeGroup(implicit signedA: Signed[A], A: AdditiveCommutativeGroup[A]) = new DefaultRuleSet(
    name = "signedAdditiveAbGroup",
    parent = Some(signed),
    "abs(x) equals abs(-x)" -> forAll { (x: A) =>
      signedA.abs(x) ?== signedA.abs(A.negate(x))
    }
  )

  // more a convention: as GCD is defined up to a unit, so up to a sign,
  // on an ordered GCD ring we require gcd(x, y) >= 0, which is the common
  // behavior of computer algebra systems
  def signedGCDRing(implicit signedA: Signed[A], A: GCDRing[A]) = new DefaultRuleSet(
    name = "signedGCDRing",
    parent = Some(signedAdditiveCommutativeGroup),
    "gcd(x, y) >= 0" -> forAll { (x: A, y: A) =>
      signedA.isSignNonNegative(A.gcd(x, y))
    },
    "gcd(x, 0) === abs(x)" -> forAll { (x: A) =>
      A.gcd(x, A.zero) ?== signedA.abs(x)
    }
  )

  def truncatedDivision(implicit ring: CommutativeRing[A], A: TruncatedDivision[A]) = new DefaultRuleSet(
    name = "truncatedDivision",
    parent = Some(signed),
    "division rule (tquotmod)" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        val (q, r) = A.tquotmod(x, y)
        x ?== ring.plus(ring.times(y, q), r)
      }
    },
    "division rule (fquotmod)" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        val (q, r) = A.fquotmod(x, y)
        x ?== ring.plus(ring.times(y, q), r)
      }
    },
    "|r| < |y| (tmod)" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        val r = A.tmod(x, y)
        A.order.lt(A.abs(r), A.abs(y))
      }
    },
    "|r| < |y| (fmod)" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        val r = A.fmod(x, y)
        A.order.lt(A.abs(r), A.abs(y))
      }
    },
    "r = 0 or sign(r) = sign(x) (tmod)" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        val r = A.tmod(x, y)
        A.isSignZero(r) || (A.sign(r) ?== A.sign(x))
      }
    },
    "r = 0 or sign(r) = sign(y) (fmod)" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        val r = A.fmod(x, y)
        A.isSignZero(r) || (A.sign(r) ?== A.sign(y))
      }
    },
    "tquot" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        A.tquotmod(x, y)._1 ?== A.tquot(x, y)
      }
    },
    "tmod" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        A.tquotmod(x, y)._2 ?== A.tmod(x, y)
      }
    },
    "fquot" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        A.fquotmod(x, y)._1 ?== A.fquot(x, y)
      }
    },
    "fmod" -> forAll { (x: A, y: A) =>
      A.isSignNonZero(y) ==> {
        A.fquotmod(x, y)._2 ?== A.fmod(x, y)
      }
    }
  )

  class OrderProperties(
    name: String,
    parent: Option[RuleSet],
    props: (String, Prop)*
  ) extends DefaultRuleSet(name, parent, props: _*)

}
