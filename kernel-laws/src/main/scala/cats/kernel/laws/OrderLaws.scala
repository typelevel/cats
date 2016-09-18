package cats.kernel
package laws

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop._

import cats.kernel.instances.all._

object OrderLaws {
  def apply[A: Eq: Arbitrary: Cogen]: OrderLaws[A] =
    new OrderLaws[A] {
      def Equ = Eq[A]
      def Arb = implicitly[Arbitrary[A]]
      def Cog = implicitly[Cogen[A]]
    }
}

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

  class OrderProperties(
    name: String,
    parent: Option[RuleSet],
    props: (String, Prop)*
  ) extends DefaultRuleSet(name, parent, props: _*)

}
