package cats.tests

import cats.{Align, Alternative, CoflatMap, Monad, Semigroupal, Traverse, TraverseFilter}
import cats.data.{NonEmptyChain, NonEmptyList, ZipList}
import cats.kernel.Semigroup
import cats.laws.discipline.{
  AlignTests,
  AlternativeTests,
  CoflatMapTests,
  CommutativeApplyTests,
  MonadTests,
  SemigroupalTests,
  SerializableTests,
  ShortCircuitingTests,
  TraverseFilterTests,
  TraverseTests
}
import cats.laws.discipline.arbitrary._
import cats.syntax.list._
import cats.syntax.show._
import cats.syntax.eq._
import org.scalacheck.Prop._

class ListSuite extends CatsSuite {

  checkAll("List[Int]", SemigroupalTests[List].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[List]", SerializableTests.serializable(Semigroupal[List]))

  checkAll("List[Int]", CoflatMapTests[List].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[List]", SerializableTests.serializable(CoflatMap[List]))

  checkAll("List[Int]", AlternativeTests[List].alternative[Int, Int, Int])
  checkAll("Alternative[List]", SerializableTests.serializable(Alternative[List]))

  checkAll("List[Int] with Option", TraverseTests[List].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[List]", SerializableTests.serializable(Traverse[List]))

  checkAll("List[Int]", MonadTests[List].monad[Int, Int, Int])
  checkAll("Monad[List]", SerializableTests.serializable(Monad[List]))

  checkAll("List[Int]", TraverseFilterTests[List].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[List]", SerializableTests.serializable(TraverseFilter[List]))

  checkAll("List[Int]", AlignTests[List].align[Int, Int, Int, Int])
  checkAll("Align[List]", SerializableTests.serializable(Align[List]))

  checkAll("List[Int]", ShortCircuitingTests[List].traverseFilter[Int])
  checkAll("List[Int]", ShortCircuitingTests[List].foldable[Int])

  checkAll("ZipList[Int]", CommutativeApplyTests[ZipList].commutativeApply[Int, Int, Int])

  test("nel => list => nel returns original nel")(
    forAll { (fa: NonEmptyList[Int]) =>
      assert(fa.toList.toNel === (Some(fa)))
    }
  )

  test("toNel on empty list returns None") {
    assert(List.empty[Int].toNel === None)
  }

  test("nec => list => nec returns original nel")(
    forAll { (fa: NonEmptyChain[Int]) =>
      assert(fa.toChain.toList.toNec === (Some(fa)))
    }
  )

  test("toNec on empty list returns None") {
    assert(List.empty[Int].toNec === None)
  }

  test("groupByNel should be consistent with List#groupBy") {
    forAll { (fa: List[Int], key: Int => Int) =>
      val result = fa.groupByNel(key).map { case (k, v) => (k, v.toList) }.toMap
      val expected = fa.groupBy(key)
      assert(result === expected)
    }
  }

  test("groupByNec should be consistent with List#groupBy") {
    forAll { (fa: List[Int], key: Int => Int) =>
      val result = fa.groupByNec(key).map { case (k, v) => (k, v.toChain.toList) }.toMap
      val expected = fa.groupBy(key)
      assert(result === expected)
    }
  }

  test("groupMapNel should be consistent with List#groupBy + Map#mapValues") {
    forAll { (fa: List[Int], key: Int => Int, f: Int => String) =>
      val result = fa.groupMapNel(key)(f).map { case (k, v) => (k, v.toList) }.toMap
      val expected = fa.groupBy(key).map { case (k, v) => (k, v.map(f)) }
      assert(result === expected)
    }
  }

  test("groupMapNec should be consistent with List#groupBy + Map#mapValues") {
    forAll { (fa: List[Int], key: Int => Int, f: Int => String) =>
      val result = fa.groupMapNec(key)(f).map { case (k, v) => (k, v.toChain.toList) }.toMap
      val expected = fa.groupBy(key).map { case (k, v) => (k, v.map(f)) }
      assert(result === expected)
    }
  }

  test("groupMapReduce should be consistent with List#groupBy + Map#mapValues + List#reduce") {
    forAll { (fa: List[String], key: String => String, f: String => Int) =>
      val result = fa.groupMapReduce(key)(f).toMap
      val expected = fa.groupBy(key).map { case (k, v) => (k, v.map(f).reduce(Semigroup[Int].combine)) }
      assert(result === expected)
    }
  }

  test("groupMapReduceWith should be consistent with List#groupBy + Map#mapValues + List#reduce") {
    forAll { (fa: List[String], key: String => String, f: String => Int, combine: (Int, Int) => Int) =>
      val result = fa.groupMapReduceWith(key)(f)(combine).toMap
      val expected = fa.groupBy(key).map { case (k, v) => (k, v.map(f).reduce(combine)) }
      assert(result === expected)
    }
  }

  test("groupByNelA should be consistent with groupByNel") {
    forAll { (fa: List[Int], f: Int => Int) =>
      assert(fa.groupByNelA(f.andThen(Option(_))) === (Option(fa.groupByNel(f))))
    }
  }

  test("show") {
    assert(List(1, 2, 3).show === "List(1, 2, 3)")
    assert((Nil: List[Int]).show === "List()")
    forAll { (l: List[String]) =>
      assert(l.show === (l.toString))
    }
  }

  test("traverse is stack-safe") {
    val lst = (0 until 100000).toList
    val sumAll = Traverse[List]
      .traverse(lst) { i => () => i }
      .apply
      .sum

    assert(sumAll == lst.sum)
  }
}

final class ListInstancesSuite extends munit.FunSuite {

  test("NonEmptyParallel instance in cats.instances.list") {
    import cats.instances.list._
    import cats.syntax.parallel._

    (List(1, 2, 3), List("A", "B", "C")).parTupled
  }
}
