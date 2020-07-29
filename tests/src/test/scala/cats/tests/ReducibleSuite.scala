package cats.tests

import cats.{Eval, NonEmptyReducible, Now, Reducible}
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.kernel.Eq
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.list._
import cats.syntax.option._
import cats.syntax.reducible._
import org.scalacheck.Arbitrary
import org.scalactic.CanEqual

import scala.collection.mutable

class ReducibleSuiteAdditional extends CatsSuite {

  test("Reducible[NonEmptyList].reduceLeftM stack safety") {
    def nonzero(acc: Long, x: Long): Option[Long] =
      if (x == 0) None else Some(acc + x)

    val n = 100000L
    val expected = n * (n + 1) / 2
    val actual = (1L to n).toList.toNel.flatMap(_.reduceLeftM(Option.apply)(nonzero))
    actual should ===(Some(expected))
  }

  test("Reducible[NonEmptyList].reduceRightTo stack safety") {
    val n = 100000L
    val actual = (1L to n).toList.toNel.get.reduceRightTo(identity) { case (a, r) => r.map(_ + a) }.value
    actual should ===((1L to n).sum)
  }

  // exists method written in terms of reduceRightTo
  def contains[F[_]: Reducible, A: Eq](as: F[A], goal: A): Eval[Boolean] =
    as.reduceRightTo(_ === goal) { (a, lb) =>
      if (a === goal) Now(true) else lb
    }

  test("Reducible[NonEmptyList] default get/size implementation") {
    testDefaultGetAndSize(1, 2, 3) { (head, tail) => NonEmptyList(head, tail.toList) }
  }

  test("Reducible[NonEmptyVector] default get/size implementation") {
    testDefaultGetAndSize(1, 2, 3) { (head, tail) => NonEmptyVector(head, tail.toVector) }
  }

  def testDefaultGetAndSize[F[_], G[_]](head: Int, tail: Int*)(
    c: (Int, Seq[Int]) => F[Int]
  )(implicit R: NonEmptyReducible[F, G]) = {
    val f: F[Int] = c(head, tail)
    R.get(f)(1L) should ===(f.get(1L))
    R.size(f) should ===(f.size.toLong)
    R.get(f)(4L) should ===(None)
  }

  test("Reducible[NonEmptyList]") {
    val R = Reducible[NonEmptyList]

    // some basic sanity checks
    val tail = (2 to 10).toList
    val total = 1 + tail.sum
    val nel = NonEmptyList(1, tail)
    R.reduceLeft(nel)(_ + _) should ===(total)
    R.reduceRight(nel)((x, ly) => ly.map(x + _)).value should ===(total)
    R.reduce(nel) should ===(total)

    // more basic checks
    val names = NonEmptyList.of("Aaron", "Betty", "Calvin", "Deirdra")
    val totalLength = names.toList.map(_.length).sum
    R.reduceLeftTo(names)(_.length)((sum, s) => s.length + sum) should ===(totalLength)
    R.reduceMap(names)(_.length) should ===(totalLength)
    val sumLeftM = R.reduceLeftM(names)(Some(_): Option[String]) { (acc, x) =>
      (Some(acc + x): Option[String])
    }
    assert(sumLeftM == Some("AaronBettyCalvinDeirdra"))
    val sumMapM = R.reduceMapM(names) { x =>
      (Some(x): Option[String])
    }
    assert(sumMapM == Some("AaronBettyCalvinDeirdra"))
    val isNotCalvin: String => Option[String] =
      x => if (x == "Calvin") None else Some(x)
    val notCalvin = R.reduceLeftM(names)(isNotCalvin) { (acc, x) =>
      isNotCalvin(x).map(acc + _)
    }
    assert(notCalvin == None)
    val notCalvinMapM = R.reduceMapM(names)(isNotCalvin)
    assert(notCalvinMapM == None)

    // test trampolining
    val large = NonEmptyList(1, (2 to 10000).toList)
    assert(contains(large, 10000).value)
  }

  test("Reducible[NonEmptyList].reduceMapA can breakout") {
    val notAllEven = NonEmptyList.of(2, 4, 6, 9, 10, 12, 14)
    val out = mutable.ListBuffer[Int]()

    notAllEven.reduceMapA { a => out += a; if (a % 2 == 0) Some(a) else None }

    out.toList should ===(List(2, 4, 6, 9))
  }

  // A simple non-empty stream with lazy `foldRight` and `reduceRightTo` implementations.
  case class NES[A](h: A, t: Stream[A]) {
    def toStream: Stream[A] = h #:: t
  }

  object NES {
    implicit val nesReducible: Reducible[NES] = new Reducible[NES] {
      def foldLeft[A, B](fa: NES[A], b: B)(f: (B, A) => B): B = fa.toStream.foldLeft(b)(f)
      def foldRight[A, B](fa: NES[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case NES(h, Stream())  => f(h, lb)
          case NES(h, th #:: tt) => f(h, Eval.defer(foldRight(NES(th, tt), lb)(f)))
        }

      def reduceLeftTo[A, B](fa: NES[A])(f: A => B)(g: (B, A) => B): B = fa.t.foldLeft(f(fa.h))(g)
      def reduceRightTo[A, B](fa: NES[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case NES(h, Stream())  => Eval.now(f(h))
          case NES(h, th #:: tt) => g(h, Eval.defer(reduceRightTo(NES(th, tt))(f)(g)))
        }
    }
  }

  test("reduceMapM should be stack-safe and short-circuiting if reduceRightTo is sufficiently lazy") {
    val n = 100000
    val xs = NES(0, Stream.from(1))

    assert(xs.reduceMapM(i => if (i < n) Right(i) else Left(i)) === Left(n))
  }

  test("reduceMapA should be stack-safe and short-circuiting if reduceRightTo is sufficiently lazy") {
    val n = 100000
    val xs = NES(0, Stream.from(1))

    assert(xs.reduceMapA(i => if (i < n) Right(i) else Left(i)) === Left(n))
  }

  test("reduceA should be stack-safe and short-circuiting if reduceRightTo is sufficiently lazy") {
    val n = 100000
    val xs = NES(Right(0), Stream.from(1).map(i => if (i < n) Right(i) else Left(i)))

    xs.reduceA should ===(Left(n))
  }
}

abstract class ReducibleSuite[F[_]: Reducible](name: String)(implicit
  ArbFInt: Arbitrary[F[Int]],
  ArbFString: Arbitrary[F[String]]
) extends FoldableSuite[F](name) {

  def range(start: Long, endInclusive: Long): F[Long]
  def fromValues[A](el: A, els: A*): F[A]

  test(s"Reducible[$name].reduceLeftM stack safety") {
    def nonzero(acc: Long, x: Long): Option[Long] =
      if (x == 0) None else Some(acc + x)

    val n = 100000L
    val expected = n * (n + 1) / 2
    val actual = range(1L, n).reduceLeftM(Option.apply)(nonzero)
    actual should ===(Some(expected))
  }

  test(s"Reducible[$name].reduceA successful case") {
    val expected = 6
    val actual = fromValues(1.asRight[String], 2.asRight[String], 3.asRight[String]).reduceA
    actual should ===(expected.asRight[String])
  }

  test(s"Reducible[$name].reduceA failure case") {
    val expected = "boom!!!"
    val actual = fromValues(1.asRight, "boom!!!".asLeft, 3.asRight).reduceA
    actual should ===(expected.asLeft[Int])
  }

  test(s"Reducible[$name].reduceMapA successful case") {
    val expected = "123"
    val actual = range(1, 3).reduceMapA(_.toString.some)

    actual should ===(expected.some)
  }

  test(s"Reducible[$name].reduceMapA failure case") {
    def intToString(i: Long): Either[String, Int] = if (i == 2) i.toInt.asRight else "boom!!!".asLeft

    val expected = "boom!!!"
    val actual = range(1, 3).reduceMapA(intToString)
    actual should ===(expected.asLeft[Int])
  }

  test(s"Reducible[$name].toNonEmptyList/toList consistency") {
    forAll { (fa: F[Int]) =>
      fa.toList.toNel should ===(Some(fa.toNonEmptyList))
    }
  }

  test(s"Reducible[$name].nonEmptyIntercalate") {
    forAll { (fa: F[String], a: String) =>
      fa.nonEmptyIntercalate(a) === (fa.toList.mkString(a))
    }
  }

  test("Reducible#nonEmptyPartition retains size") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val folded = fi.nonEmptyPartition(f).fold(identity, identity, _ ++ _.toList)
      folded.size.toLong should ===(fi.size)
    }
  }

  test("Reducible#nonEmptyPartition to one side is identity") {
    forAll { (fi: F[Int], f: Int => String) =>
      val g: Int => Either[Double, String] = f.andThen(Right.apply)
      val h: Int => Either[String, Double] = f.andThen(Left.apply)

      val withG = fi.nonEmptyPartition(g).right.getOrElse(NonEmptyList.one(""))
      withG should ===(Reducible[F].toNonEmptyList(fi).map(f))

      val withH = fi.nonEmptyPartition(h).left.getOrElse(NonEmptyList.one(""))
      withH should ===(Reducible[F].toNonEmptyList(fi).map(f))
    }
  }

}
