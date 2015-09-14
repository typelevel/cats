package cats
package tests

import cats.data.{Xor, Ior}
import cats.laws.discipline.{TraverseTests, MonadTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Prop.BooleanOperators

class IorTests extends CatsSuite {
  checkAll("Ior[String, Int]", MonadTests[String Ior ?].monad[Int, Int, Int])
  checkAll("Monad[String Ior ?]]", SerializableTests.serializable(Monad[String Ior ?]))

  checkAll("Ior[String, Int] with Option", TraverseTests[String Ior ?].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[String Ior ?]", SerializableTests.serializable(Traverse[String Ior ?]))

  check {
    forAll { (i: Int Ior String) =>
      (i.isLeft || i.isBoth) == i.left.isDefined
    }
  }

  check {
    forAll { (i: Int Ior String) =>
      (i.isRight || i.isBoth) == i.right.isDefined
    }
  }

  check {
    forAll { (i: Int Ior String) =>
      i.onlyLeft.map(Xor.left).orElse(i.onlyRight.map(Xor.right)) == i.onlyLeftOrRight
    }
  }

  check {
    forAll { (i: Int Ior String) =>
      i.onlyBoth == (for {
        left <- i.left
        right <- i.right
      } yield (left, right))
    }
  }

  check {
    forAll { (i: Int Ior String) =>
      i.pad == ((i.left, i.right))
    }
  }

  check {
    forAll { (i: Int Ior String) =>
      i.unwrap.isRight == i.isBoth
    }
  }

  check {
    forAll { (i: Int Ior String) =>
      i.isLeft == i.toOption.isEmpty
    }
  }

  check {
    forAll { (i: Int Ior String) =>
      i.isLeft == i.toList.isEmpty
    }
  }

  check {
    forAll { (i: Int Ior String, p: String => Boolean) =>
      i.isLeft ==> (i.forall(p) && !i.exists(p))
    }
  }

  check {
    forAll { (i: Int Ior String, f: Int => Double) =>
      i.leftMap(f).swap == i.swap.map(f)
    }
  }

  check {
    forAll { (i: Int) =>
      Ior.left[Int, String](i).foreach { _ => fail("should not be called") }
      true
    }
  }

  check {
    forAll { (i: Int Ior String) =>
      (i.isRight || i.isBoth) ==> {
        var count = 0
        i.foreach { _ => count += 1 }
        count == 1
      }
    }
  }

  check {
    val iorShow = implicitly[Show[Int Ior String]]

    forAll { (i: Int Ior String) =>
      iorShow.show(i).size > 0
    }
  }

  check {
    forAll { (i: Int Ior String, j: Int Ior String) =>
      i.append(j).left == i.left.map(_ + j.left.getOrElse(0)).orElse(j.left)
    }
  }

  check {
    forAll { (i: Int Ior String, j: Int Ior String) =>
      i.append(j).right == i.right.map(_ + j.right.getOrElse("")).orElse(j.right)
    }
  }
}
