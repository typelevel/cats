package cats.tests

import cats.{Align, Alternative, CoflatMap, Monad, Semigroupal, Traverse, TraverseFilter}
import cats.data.{NonEmptySeq, ZipSeq}
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
import cats.syntax.show._
import cats.syntax.seq._
import cats.syntax.eq._
import org.scalacheck.Prop._
import scala.collection.immutable.Seq

class SeqSuite extends CatsSuite {
  checkAll("Seq[Int]", SemigroupalTests[Seq].semigroupal[Int, Int, Int])
  checkAll("Semigroupal[Seq]", SerializableTests.serializable(Semigroupal[Seq]))

  checkAll("Seq[Int]", CoflatMapTests[Seq].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Seq]", SerializableTests.serializable(CoflatMap[Seq]))

  checkAll("Seq[Int]", AlternativeTests[Seq].alternative[Int, Int, Int])
  checkAll("Alternative[Seq]", SerializableTests.serializable(Alternative[Seq]))

  checkAll("Seq[Int] with Option", TraverseTests[Seq].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[Seq]", SerializableTests.serializable(Traverse[Seq]))

  checkAll("Seq[Int]", MonadTests[Seq].monad[Int, Int, Int])
  checkAll("Monad[Seq]", SerializableTests.serializable(Monad[Seq]))

  checkAll("Seq[Int]", TraverseFilterTests[Seq].traverseFilter[Int, Int, Int])
  checkAll("TraverseFilter[Seq]", SerializableTests.serializable(TraverseFilter[Seq]))

  checkAll("Seq[Int]", AlignTests[Seq].align[Int, Int, Int, Int])
  checkAll("Align[Seq]", SerializableTests.serializable(Align[Seq]))

  checkAll("Seq[Int]", ShortCircuitingTests[Seq].traverseFilter[Int])
  checkAll("Seq[Int]", ShortCircuitingTests[Seq].foldable[Int])

  checkAll("ZipSeq[Int]", CommutativeApplyTests[ZipSeq].commutativeApply[Int, Int, Int])

  test("show") {
    forAll { (seq: Seq[String]) =>
      assert(seq.show === (seq.toString))
    }
  }

  test("neSeq => Seq => neSeq returns original neSeq")(
    forAll { (fa: NonEmptySeq[Int]) =>
      assert(fa.toSeq.toNeSeq == Some(fa))
    }
  )

  test("toNeSeq on empty Seq returns None") {
    assert(Seq.empty[Int].toNeSeq == None)
  }

  test("traverse is stack-safe") {
    val seq = (0 until 100000).toSeq
    val sumAll = Traverse[Seq]
      .traverse(seq) { i => () => i }
      .apply()
      .sum

    assert(sumAll == seq.sum)
  }
}

final class SeqInstancesSuite extends munit.FunSuite {

  test("NonEmptyParallel instance in cats.instances.seq") {
    import cats.instances.seq._
    import cats.syntax.parallel._

    (Seq(1, 2, 3), Seq("A", "B", "C")).parTupled
  }
}
