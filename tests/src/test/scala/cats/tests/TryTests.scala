package cats
package tests

import cats.laws.{ApplicativeLaws, CoflatMapLaws, FlatMapLaws, MonadLaws}
import cats.laws.discipline._

import scala.util.{Success, Try}

class TryTests extends CatsSuite {
  implicit val eqThrow: Eq[Throwable] = Eq.allEqual

  checkAll("Try[Int]", CartesianTests[Try].cartesian[Int, Int, Int])
  checkAll("Cartesian[Try]", SerializableTests.serializable(Cartesian[Try]))

  checkAll("Try[Int]", CoflatMapTests[Try].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Try]", SerializableTests.serializable(CoflatMap[Try]))

  checkAll("Try with Throwable", MonadErrorTests[Try, Throwable].monadError[Int, Int, Int])
  checkAll("MonadError[Try, Throwable]", SerializableTests.serializable(MonadError[Try, Throwable]))

  checkAll("Try[Int] with Option", TraverseTests[Try].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Try]", SerializableTests.serializable(Traverse[Try]))

  checkAll("Try", MonadRecTests[Try].monadRec[Int, Int, Int])
  checkAll("MonadRec[Try]", SerializableTests.serializable(MonadRec[Try]))

  test("show") {
    forAll { fs: Try[String] =>
      fs.show should === (fs.toString)
    }
  }

  // The following tests check laws which are a different formulation of
  // laws that are checked. Since these laws are more or less duplicates of
  // existing laws, we don't check them for all types that have the relevant
  // instances.

  test("Kleisli associativity") {
    forAll { (l: Long,
              f: Long => Try[Int],
              g: Int  => Try[Char],
              h: Char => Try[String]) =>
      val isEq = FlatMapLaws[Try].kleisliAssociativity(f, g, h, l)
      isEq.lhs should === (isEq.rhs)
    }
  }

  test("Cokleisli associativity") {
    forAll { (l: Try[Long],
              f: Try[Long] => Int,
              g: Try[Int]  => Char,
              h: Try[Char] => String) =>
      val isEq = CoflatMapLaws[Try].cokleisliAssociativity(f, g, h, l)
      isEq.lhs should === (isEq.rhs)
    }
  }

  test("applicative composition") {
    forAll { (fa: Try[Int],
              fab: Try[Int => Long],
              fbc: Try[Long => Char]) =>
      val isEq = ApplicativeLaws[Try].applicativeComposition(fa, fab, fbc)
      isEq.lhs should === (isEq.rhs)
    }
  }

  val monadLaws = MonadLaws[Try]

  test("Kleisli left identity") {
    forAll { (a: Int, f: Int => Try[Long]) =>
      val isEq = monadLaws.kleisliLeftIdentity(a, f)
      isEq.lhs should === (isEq.rhs)
    }
  }

  test("Kleisli right identity") {
    forAll { (a: Int, f: Int => Try[Long]) =>
      val isEq = monadLaws.kleisliRightIdentity(a, f)
      isEq.lhs should === (isEq.rhs)
    }
  }

  test("map2Eval is lazy") {
    var evals = 0
    val bomb: Eval[Try[Int]] = Later { evals += 1; Success(1) }
    Try[Int](sys.error("boom0")).map2Eval(bomb)(_ + _).value
    evals should === (0)
  }
}
