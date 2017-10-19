package cats
package tests

import cats.data.Const
import org.scalactic.CanEqual

class ContravariantSuite extends CatsSuite {

  test("narrow equals contramap(identity)") {
    implicit val constInst = Const.catsDataContravariantForConst[Int]
    implicit val canEqual: CanEqual[cats.data.Const[Int,Some[Int]],cats.data.Const[Int,Some[Int]]] =
      StrictCatsEquality.lowPriorityConversionCheckedConstraint
    forAll { (i: Int) =>
      val const: Const[Int, Option[Int]] = Const[Int, Option[Int]](i)
      val narrowed: Const[Int, Some[Int]] = constInst.narrow[Option[Int], Some[Int]](const)
      narrowed should === (constInst.contramap(const)(identity[Option[Int]](_: Some[Int])))
      assert(narrowed eq const)
    }
  }

}
