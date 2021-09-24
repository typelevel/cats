package cats.tests

import cats._
import cats.data.NonEmptySeq
import cats.data.OneAnd
import cats.data.ZipSeq
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.syntax.eq._
import cats.syntax.functor._
import cats.syntax.seq._
import cats.syntax.show._
import cats.syntax.traverse._
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

  test("concatNeSeq should be consistent with Seq#`++`") {
    forAll { (fa: Seq[Int], neseq: NonEmptySeq[Int]) =>
      // Note: Scala 2.12.x does not have `Seq#concat`.
      assert(fa.concatNeSeq(neseq).toSeq === (fa ++ neseq.toSeq))
    }
  }

  test("groupByNeSeq should be consistent with groupBy") {
    forAll { (fa: Seq[String], f: String => Int) =>
      assert((fa.groupByNeSeq(f).map { _.map(_.toSeq) }: Map[Int, Seq[String]]) === fa.groupBy(f))
    }
  }

  test("groupByNeSeqA[Id] should be consistent with groupByNeSeq") {
    forAll { (fa: Seq[String], f: String => Id[Int]) =>
      assert(fa.groupByNeSeqA(f) === fa.groupByNeSeq(f))
    }
  }

  def unwrapGroupByNeSeq[G[_]: Applicative, A, B](fa: Seq[A], f: A => G[B])(implicit ord: Order[B]) = {
    implicit val ordering = ord.toOrdering
    fa
      .traverse(a => f(a).tupleRight(a))
      .map(_.groupByNeSeq(_._1).map(_.map(_.map(_._2))))
  }

  test("groupByNeSeqA[Option] should be consistent with groupByNeSeq") {
    forAll { (fa: Seq[String], f: String => Option[Int]) =>
      assert(fa.groupByNeSeqA(f) === unwrapGroupByNeSeq(fa, f))
    }
  }
  test("groupByNeSeqA[OneAnd] should be consistent with groupByNeSeq") {
    forAll { (fa: Seq[String], f: String => OneAnd[Option, Int]) =>
      assert(fa.groupByNeSeqA(f) === unwrapGroupByNeSeq(fa, f))
    }
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
