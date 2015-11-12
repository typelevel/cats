package cats.tests

import algebra.Eq
import cats._
import cats.data.Coproduct
import cats.functor.Contravariant
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class CoproductTests extends CatsSuite {

  checkAll("Coproduct[Option, Option, ?]", TraverseTests[Coproduct[Option, Option, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Coproduct[Option, Option, ?]]", SerializableTests.serializable(Traverse[Coproduct[Option, Option, ?]]))

  checkAll("Coproduct[Eval, Eval, ?]", ComonadTests[Coproduct[Eval, Eval, ?]].comonad[Int, Int, Int])
  checkAll("Comonad[Coproduct[Eval, Eval, ?]]", SerializableTests.serializable(Comonad[Coproduct[Eval, Eval, ?]]))

  implicit def showEq[A](implicit arbA: Arbitrary[A], stringEq: Eq[String]): Eq[Show[A]] = new Eq[Show[A]] {
    def eqv(f: Show[A], g: Show[A]): Boolean = {
      val samples = List.fill(100)(arbA.arbitrary.sample).collect {
        case Some(a) => a
        case None => sys.error("Could not generate arbitrary values to compare two Show[A]")
      }
      samples.forall(s => stringEq.eqv(f.show(s), g.show(s)))
    }
  }

  checkAll("Coproduct[Show, Show, ?]", ContravariantTests[Coproduct[Show, Show, ?]].contravariant[Int, Int, Int])
  checkAll("Contravariant[Coproduct[Show, Show, ?]]", SerializableTests.serializable(Contravariant[Coproduct[Show, Show, ?]]))

  test("double swap is identity") {
    forAll { (x: Coproduct[Option, Option, Int]) =>
      x.swap.swap should ===(x)
    }
  }

}
