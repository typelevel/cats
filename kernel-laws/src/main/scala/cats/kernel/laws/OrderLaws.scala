package cats.kernel
package laws

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import cats.kernel.instances.boolean._

object OrderLaws {
  def apply[A: Eq: Arbitrary]: OrderLaws[A] = new OrderLaws[A] {
    def Equ = Eq[A]
    def Arb = implicitly[Arbitrary[A]]
  }
}

trait OrderLaws[A] extends Laws {

  implicit def Equ: Eq[A]
  implicit def Arb: Arbitrary[A]

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
    }
  )

  def order(implicit A: Order[A]): OrderProperties = new OrderProperties(
    name = "order",
    parent = Some(partialOrder),
    "totality" -> forAll { (x: A, y: A) =>
      A.lteqv(x, y) ?|| A.lteqv(y, x)
    }
  )

  class OrderProperties(
    name: String,
    parent: Option[RuleSet],
    props: (String, Prop)*
  ) extends DefaultRuleSet(name, parent, props: _*)

}
