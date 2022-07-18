package cats.tests

import cats.data.NonEmptySeq
import cats.laws.discipline.arbitrary._
import cats.syntax.seq._
import org.scalacheck.Prop._

import scala.collection.immutable.Seq

class NonEmtpySeqSuite extends NonEmptyCollectionSuite[Seq, NonEmptySeq, NonEmptySeq]{
  protected def toList[A](value: NonEmptySeq[A]): List[A] = value.toSeq.toList
  protected def underlyingToList[A](underlying: Seq[A]): List[A] = underlying.toList
  protected def toNonEmptyCollection[A](value: NonEmptySeq[A]): NonEmptySeq[A] = value

  test("neSeq => Seq => neSeq returns original neSeq")(
    forAll { (fa: NonEmptySeq[Int]) =>
      assert(fa.toSeq.toNeSeq == Some(fa))
    }
  )

  test("NonEmptySeq#concat/concatNeSeq/appendSeq is consistent with Seq#++")(
    forAll { (fa: NonEmptySeq[Int], fb: Seq[Int], n: Int) =>
      assert((fa ++ fb).toSeq == fa.toSeq ++ fb)
      assert(fa.concat(fb).toSeq == fa.toSeq ++ fb)
      assert(fa.appendSeq(fb).toSeq == fa.toSeq ++ fb)
      assert(fa.concatNeSeq(NonEmptySeq(n, fb)).toSeq == fa.toSeq :+ n ++: fb)
    }
  )

  test("toNeSeq on empty Seq returns None") {
    assert(Seq.empty[Int].toNeSeq == None)
  }
}
