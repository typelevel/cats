package cats.tests

import cats._
import cats.kernel.laws.OrderLaws
import cats.data.Coproduct
import cats.functor.Contravariant
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary

class CoproductTests extends CatsSuite {

  checkAll("Coproduct[Option, Option, ?]", TraverseTests[Coproduct[Option, Option, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Coproduct[Option, Option, ?]]", SerializableTests.serializable(Traverse[Coproduct[Option, Option, ?]]))

  {
    implicit val foldable = Coproduct.coproductFoldable[Option, Option]
    checkAll("Coproduct[Option, Option, ?]", FoldableTests[Coproduct[Option, Option, ?]].foldable[Int, Int])
    checkAll("Foldable[Coproduct[Option, Option, ?]]", SerializableTests.serializable(Foldable[Coproduct[Option, Option, ?]]))
  }

  checkAll("Coproduct[Eval, Eval, ?]", ComonadTests[Coproduct[Eval, Eval, ?]].comonad[Int, Int, Int])
  checkAll("Comonad[Coproduct[Eval, Eval, ?]]", SerializableTests.serializable(Comonad[Coproduct[Eval, Eval, ?]]))

  {
    implicit val coflatMap = Coproduct.coproductCoflatMap[Eval, Eval]
    checkAll("Coproduct[Eval, Eval, ?]", CoflatMapTests[Coproduct[Eval, Eval, ?]].coflatMap[Int, Int, Int])
    checkAll("CoflatMap[Coproduct[Eval, Eval, ?]]", SerializableTests.serializable(CoflatMap[Coproduct[Eval, Eval, ?]]))
  }

  checkAll("Coproduct[Option, Option, Int]", OrderLaws[Coproduct[Option, Option, Int]].eqv)
  checkAll("Eq[Coproduct[Option, Option, Int]]", SerializableTests.serializable(Eq[Coproduct[Option, Option, Int]]))

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

  test("swap negates isLeft/isRight") {
    forAll { (x: Coproduct[Option, Option, Int]) =>
      x.isLeft should !== (x.swap.isLeft)
      x.isRight should !== (x.swap.isRight)
    }
  }

  test("isLeft consistent with isRight") {
    forAll { (x: Coproduct[Option, Option, Int]) =>
      x.isLeft should !== (x.isRight)
    }
  }

  test("toValidated + toXor is identity") {
    forAll { (x: Coproduct[Option, List, Int]) =>
      x.toValidated.toXor should === (x.run)
    }
  }
}
