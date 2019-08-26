package cats.tests

import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{
  AlternativeTests,
  CoflatMapTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.{Alternative, CoflatMap, Monad, Semigroupal, Traverse, TraverseFilter}

import scala.collection.immutable.Seq

class SeqSuite extends CatsSuite {

  checkAll("Seq[Int]", SemigroupalTests[Seq].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Seq]", SerializableTests.serializable(Semigroupal[Seq]))

  checkAll("Seq[Seq]", CoflatMapTests[Seq].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Seq]", SerializableTests.serializable(CoflatMap[Seq]))

  checkAll("Seq[Int]", AlternativeTests[Seq].alternative[Int, Int, Int])
  checkAll("Alternative[Seq]", SerializableTests.serializable(Alternative[Seq]))

  checkAll("Seq[Int] with Option", TraverseTests[Seq].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[Seq]", SerializableTests.serializable(Traverse[Seq]))

  checkAll("Seq[Int]", MonadTests[Seq].monad[Int, Int, Int])
  checkAll("Monad[Seq]", SerializableTests.serializable(Monad[Seq]))

  checkAll("Seq[Int]", TraverseFilterTests[Seq].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Seq]", SerializableTests.serializable(TraverseFilter[Seq]))

  // checkAll("ZipSeq[Int]", CommutativeApplyTests[ZipList].commutativeApply[Int, Int, Int])

  test("nel => seq => nel returns original nel")(
    forAll { fa: NonEmptyList[Int] =>
      val s: Seq[Int] = fa.toList.toSeq
      s.toNel should ===(Some(fa))
    }
  )

  test("toNel on empty seq returns None") {
    Seq.empty[Int].toNel should ===(None)
  }

  test("groupByNel should be consistent with groupBy")(
    forAll { (fa: Seq[Int], f: Int => Int) =>
      fa.groupByNel(f).map { case (k, v) => (k, v.toList) } should ===(fa.groupBy(f))
    }
  )

  test("show") {
    Seq(1, 2, 3).show should ===("Seq(1, 2, 3)")
    (Nil: Seq[Int]).show should ===("Seq()")
    forAll { l: Seq[String] =>
      l.show should ===(l.mkString("Seq(", ", ", ")"))
    }
  }
}
