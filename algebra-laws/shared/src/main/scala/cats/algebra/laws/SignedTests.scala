package cats.algebra.laws

import cats.algebra.ring.{AdditiveCommutativeGroup, CommutativeRing, GCDRing, Signed, TruncatedDivision}
import cats.kernel._
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop._
import cats.kernel.instances.all._
import cats.kernel.laws.OrderLaws
import cats.kernel.laws.discipline.OrderTests

object SignedTests {
  def apply[A: Order: Arbitrary: Cogen]: SignedTests[A] =
    new SignedTests[A] {
      def Arb = implicitly[Arbitrary[A]]
      def Cog = Cogen[A]
      def Ord = Order[A]
      def laws: OrderLaws[A] = OrderLaws[A]
    }
}

trait SignedTests[A] extends OrderTests[A] {

  implicit def Arb: Arbitrary[A]
  implicit def Cog: Cogen[A]
  implicit def Ord: Order[A]

  def signed(implicit A: Signed[A]) = new OrderProperties(
    name = "signed",
    parent = Some(order),
    "abs non-negative" -> forAll((x: A) => A.sign(A.abs(x)) != Signed.Negative),
    "abs never less than" -> forAll((x: A) => A.order.gteqv(A.abs(x), x)),
    "signum returns -1/0/1" -> forAll((x: A) => A.signum(A.abs(x)) <= 1),
    "signum is sign.toInt" -> forAll((x: A) => A.signum(x) == A.sign(x).toInt),
    "linear order" -> forAll { (x: A, y: A, z: A) =>
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
