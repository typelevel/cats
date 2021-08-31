package cats.tests

import cats.{CoflatMap, Eval, Later, Monad, MonadThrow, Semigroupal, Traverse}
import cats.kernel.{Eq, Monoid, Semigroup}
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.{ApplicativeLaws, CoflatMapLaws, FlatMapLaws, MonadLaws}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.apply._
import cats.syntax.show._
import scala.util.{Success, Try}
import cats.syntax.eq._
import org.scalacheck.Prop._

class TrySuite extends CatsSuite {
  implicit val eqThrow: Eq[Throwable] = Eq.allEqual

  checkAll("Try[Int]", SemigroupalTests[Try].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Try]", SerializableTests.serializable(Semigroupal[Try]))

  checkAll("Try[Int]", CoflatMapTests[Try].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Try]", SerializableTests.serializable(CoflatMap[Try]))

  checkAll("Try with Throwable", MonadErrorTests[Try, Throwable].monadError[Int, Int, Int])
  checkAll("MonadThrow[Try]", SerializableTests.serializable(MonadThrow[Try]))

  checkAll("Try[Int] with Option", TraverseTests[Try].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Try]", SerializableTests.serializable(Traverse[Try]))

  checkAll("Try", MonadTests[Try].monad[Int, Int, Int])
  checkAll("Monad[Try]", SerializableTests.serializable(Monad[Try]))

  {
    implicit val F: Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]

    checkAll("Try[ListWrapper[Int]]", SemigroupTests[Try[ListWrapper[Int]]].semigroup)
    checkAll("Semigroup[Try[ListWrapper[Int]]", SerializableTests.serializable(Semigroup[Try[ListWrapper[Int]]]))
  }

  checkAll("Try[Int]", MonoidTests[Try[Int]].monoid)
  checkAll("Monoid[Try[Int]]", SerializableTests.serializable(Monoid[Try[Int]]))

  test("show") {
    forAll { (fs: Try[String]) =>
      assert(fs.show === (fs.toString))
    }
  }

  test("catchNonFatal works") {
    forAll { (e: Either[String, Int]) =>
      val str = e.fold(identity, _.toString)
      val res = MonadThrow[Try].catchNonFatal(str.toInt)
      // the above should just never cause an uncaught exception
      // this is a somewhat bogus test:
      assert(res != null)
    }
  }

  test("catchNonFatalEval works") {
    forAll { (e: Either[String, Int]) =>
      val str = e.fold(identity, _.toString)
      val res = MonadThrow[Try].catchNonFatalEval(Eval.later(str.toInt))
      // the above should just never cause an uncaught exception
      // this is a somewhat bogus test:
      assert(res != null)
    }
  }

  test("catchOnly works") {
    forAll { (e: Either[String, Int]) =>
      val str = e.fold(identity, _.toString)
      val res = MonadThrow[Try].catchOnly[NumberFormatException](str.toInt)
      // the above should just never cause an uncaught exception
      // this is a somewhat bogus test:
      assert(res != null)
    }
  }

  test("catchOnly catches only a specified type") {
    intercept[NumberFormatException] {
      MonadThrow[Try].catchOnly[UnsupportedOperationException]("str".toInt)
    }
  }

  test("fromTry works") {
    forAll { (t: Try[Int]) =>
      assert((MonadThrow[Try].fromTry(t)) === t)
    }
  }

  // The following tests check laws which are a different formulation of
  // laws that are checked. Since these laws are more or less duplicates of
  // existing laws, we don't check them for all types that have the relevant
  // instances.

  test("Kleisli associativity") {
    forAll { (l: Long, f: Long => Try[Int], g: Int => Try[Char], h: Char => Try[String]) =>
      val isEq = FlatMapLaws[Try].kleisliAssociativity(f, g, h, l)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  test("Cokleisli associativity") {
    forAll { (l: Try[Long], f: Try[Long] => Int, g: Try[Int] => Char, h: Try[Char] => String) =>
      val isEq = CoflatMapLaws[Try].cokleisliAssociativity(f, g, h, l)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  test("applicative composition") {
    forAll { (fa: Try[Int], fab: Try[Int => Long], fbc: Try[Long => Char]) =>
      val isEq = ApplicativeLaws[Try].applicativeComposition(fa, fab, fbc)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  val monadLaws = MonadLaws[Try]

  test("Kleisli left identity") {
    forAll { (a: Int, f: Int => Try[Long]) =>
      val isEq = monadLaws.kleisliLeftIdentity(a, f)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  test("Kleisli right identity") {
    forAll { (a: Int, f: Int => Try[Long]) =>
      val isEq = monadLaws.kleisliRightIdentity(a, f)
      assert(isEq.lhs === (isEq.rhs))
    }
  }

  test("map2Eval is lazy") {
    var evals = 0
    val bomb: Eval[Try[Int]] = Later { evals += 1; Success(1) }
    Try[Int](sys.error("boom0")).map2Eval(bomb)(_ + _).value
    assert(evals === 0)
  }
}
