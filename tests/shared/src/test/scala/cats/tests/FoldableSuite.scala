/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats._
import cats.data._
import cats.instances.order._
import cats.kernel.compat.scalaVersionSpecific._
import cats.laws.discipline.arbitrary._
import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.util.Try

@suppressUnusedImportWarningForScalaVersionSpecific
abstract class FoldableSuite[F[_]: Foldable](name: String)(implicit
  ArbFInt: Arbitrary[F[Int]],
  ArbFString: Arbitrary[F[String]]
) extends CatsSuite {

  def iterator[T](fa: F[T]): Iterator[T]

  test(s"Foldable[$name].size/get") {
    forAll { (fa: F[Int], n: Int) =>
      val s = fa.size
      assert(s === (iterator(fa).size.toLong))
      if (n < s && n >= 0) {
        fa.get(n.toLong) === Some(iterator(fa).take(n + 1).toList.last)
      } else {
        fa.get(n.toLong) === None
      }
    }
  }

  test("Foldable#partitionEither retains size") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val vector = Foldable[F].toList(fi).toVector
      val (lefts, rights) = Foldable[Vector].partitionEither(vector)(f)
      assert((lefts <+> rights).size.toLong === (fi.size))
    }
  }

  test("Foldable#partitionEither consistent with List#partition") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val list = Foldable[F].toList(fi)
      val (lefts, rights) = Foldable[List].partitionEither(list)(f)
      val (ls, rs) = list
        .map(f)
        .partition {
          case Left(_)  => true
          case Right(_) => false
        }

      assert(lefts.map(_.asLeft[String]) === ls)
      assert(rights.map(_.asRight[String]) === rs)
    }
  }

  test("Foldable#partitionEither to one side is identity") {
    forAll { (fi: F[Int], f: Int => String) =>
      val list = Foldable[F].toList(fi)
      val g: Int => Either[Double, String] = f.andThen(Right.apply)
      val h: Int => Either[String, Double] = f.andThen(Left.apply)

      val withG = Foldable[List].partitionEither(list)(g)._2
      assert(withG === (list.map(f)))

      val withH = Foldable[List].partitionEither(list)(h)._1
      assert(withH === (list.map(f)))
    }
  }

  test("Foldable#partitionEither remains sorted") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val list = Foldable[F].toList(fi)

      val sorted = list.map(f).sorted
      val (lefts, rights) = Foldable[List].partitionEither(sorted)(identity)

      assert(lefts.sorted === lefts)
      assert(rights.sorted === rights)
    }
  }

  test("Foldable#partitionEitherM retains size") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val vector = Foldable[F].toList(fi).toVector
      val result = Foldable[Vector].partitionEitherM(vector)(f.andThen(Option.apply)).map { case (lefts, rights) =>
        (lefts <+> rights).size
      }
      assert(result === (Option(vector.size)))
    }
  }

  test("Foldable#partitionEitherM consistent with List#partition") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val list = Foldable[F].toList(fi)
      val partitioned = Foldable[List].partitionEitherM(list)(f.andThen(Option.apply))
      val (ls, rs) = list
        .map(f)
        .partition {
          case Left(_)  => true
          case Right(_) => false
        }

      assert(partitioned.map(_._1.map(_.asLeft[String])) === (Option(ls)))
      assert(partitioned.map(_._2.map(_.asRight[String])) === (Option(rs)))
    }
  }

  test("Foldable#partitionEitherM to one side is identity") {
    forAll { (fi: F[Int], f: Int => String) =>
      val list = Foldable[F].toList(fi)
      val g: Int => Option[Either[Double, String]] = f.andThen(Right.apply).andThen(Option.apply)
      val h: Int => Option[Either[String, Double]] = f.andThen(Left.apply).andThen(Option.apply)

      val withG = Foldable[List].partitionEitherM(list)(g).map(_._2)
      assert(withG === (Option(list.map(f))))

      val withH = Foldable[List].partitionEitherM(list)(h).map(_._1)
      assert(withH === (Option(list.map(f))))
    }
  }

  test("Foldable#partitionEitherM remains sorted") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val list = Foldable[F].toList(fi)

      val sorted = list.map(f).sorted
      val pairs = Foldable[List].partitionEitherM(sorted)(Option.apply)

      assert(pairs.map(_._1.sorted) === (pairs.map(_._1)))
      assert(pairs.map(_._2.sorted) === (pairs.map(_._2)))
    }
  }

  test(s"Foldable[$name] summation") {
    forAll { (fa: F[Int]) =>
      val total = iterator(fa).sum
      assert(fa.foldLeft(0)(_ + _) === total)
      assert(fa.foldRight(Now(0))((x, ly) => ly.map(x + _)).value === total)
      assert(fa.fold === total)
      assert(fa.foldMap(identity) === total)
    }
  }

  test(s"Foldable[$name] partial summation") {
    forAll { (fa: F[String], f: String => Boolean) =>
      val m: Monoid[String] = Monoid[String]

      val pf: PartialFunction[String, String] = {
        case n if f(n) => n
      }
      assert(fa.collectFold(pf) === (fa.toList.collect(pf).fold(m.empty)(m.combine)))

      def g(a: String): Option[String] = Some(a).filter(f)

      // `collectSomeFold` (deprecated) is used here instead of `collectFoldSome` to
      // keep testing the deprecated code until it's finally removed. This helps with
      // the coverage and, most importantly, prevents from breaking deprecated code paths
      // that might still be in use.
      //
      // https://github.com/typelevel/cats/pull/3278#discussion_r372841693
      @annotation.nowarn("cat=deprecation")
      val obtained = fa.collectSomeFold(g)
      assert(obtained === (fa.toList.filter(f).fold(m.empty)(m.combine)))
    }
  }

  test(s"Foldable[$name].find/exists/forall/findM/existsM/forallM/filter_/dropWhile_") {
    forAll { (fa: F[Int], n: Int) =>
      assert(fa.find(_ > n) === (iterator(fa).find(_ > n)))
      assert(fa.exists(_ > n) === (iterator(fa).exists(_ > n)))
      assert(fa.forall(_ > n) === (iterator(fa).forall(_ > n)))
      assert(fa.findM(k => Option(k > n)) === (Option(iterator(fa).find(_ > n))))
      assert(fa.existsM(k => Option(k > n)) === (Option(iterator(fa).exists(_ > n))))
      assert(fa.forallM(k => Option(k > n)) === (Option(iterator(fa).forall(_ > n))))
      assert(fa.filter_(_ > n) === (iterator(fa).filter(_ > n).toList))
      assert(fa.dropWhile_(_ > n) === (iterator(fa).dropWhile(_ > n).toList))
      assert(fa.takeWhile_(_ > n) === (iterator(fa).takeWhile(_ > n).toList))
    }
  }

  test(s"Foldable[$name].toList/isEmpty/nonEmpty") {
    forAll { (fa: F[Int]) =>
      assert(fa.toList === (iterator(fa).toList))
      assert(fa.isEmpty === (iterator(fa).isEmpty))
      assert(fa.nonEmpty === (iterator(fa).nonEmpty))
    }
  }

  test(s"Foldable[$name].maximum/minimum") {
    forAll { (fa: F[Int]) =>
      val maxOpt = fa.maximumOption
      val minOpt = fa.minimumOption
      val maxList = fa.maximumList
      val minList = fa.minimumList
      val list = fa.toList
      val nelOpt = list.toNel
      assert(maxOpt === nelOpt.map(_.maximum))
      assert(maxOpt === nelOpt.map(_.toList.max))
      assert(maxList.lastOption === nelOpt.map(_.maximum))
      assert(maxList.lastOption === nelOpt.map(_.toList.max))
      assert(minOpt === nelOpt.map(_.minimum))
      assert(minOpt === nelOpt.map(_.toList.min))
      assert(minList.lastOption === nelOpt.map(_.minimum))
      assert(minList.lastOption === nelOpt.map(_.toList.min))
      assert(maxOpt.forall(i => fa.forall(_ <= i)))
      assert(minOpt.forall(i => fa.forall(_ >= i)))
      assert(maxList.forall(i => fa.forall(_ <= i)))
      assert(minList.forall(i => fa.forall(_ >= i)))
      assert(maxList.flatMap(a => maxList.map(b => a -> b)).forall { case (a, b) => a === b })
      assert(minList.flatMap(a => minList.map(b => a -> b)).forall { case (a, b) => a === b })
    }
  }

  test(s"Foldable[$name].maximumBy/minimumBy") {
    forAll { (fa: F[Int], f: Int => Int) =>
      val maxOpt = fa.maximumByOption(f).map(f)
      val minOpt = fa.minimumByOption(f).map(f)
      val maxList = fa.maximumByList(f).map(f)
      val minList = fa.minimumByList(f).map(f)
      val nelOpt = fa.toList.toNel
      assert(maxOpt === nelOpt.map(_.maximumBy(f)).map(f))
      assert(maxOpt === nelOpt.map(_.toList.maxBy(f)).map(f))
      assert(maxList.lastOption === nelOpt.map(_.maximumBy(f)).map(f))
      assert(maxList.lastOption === nelOpt.map(_.toList.maxBy(f)).map(f))
      assert(minOpt === nelOpt.map(_.minimumBy(f)).map(f))
      assert(minOpt === nelOpt.map(_.toList.minBy(f)).map(f))
      assert(minList.lastOption === nelOpt.map(_.minimumBy(f)).map(f))
      assert(minList.lastOption === nelOpt.map(_.toList.minBy(f)).map(f))
      assert(maxOpt.forall(i => fa.forall(f(_) <= i)))
      assert(minOpt.forall(i => fa.forall(f(_) >= i)))
      assert(maxList.forall(i => fa.forall(f(_) <= i)))
      assert(minList.forall(i => fa.forall(f(_) >= i)))
      assert(maxList.flatMap(a => maxList.map(b => a -> b)).forall { case (a, b) => f(a) === f(b) })
      assert(minList.flatMap(a => minList.map(b => a -> b)).forall { case (a, b) => f(a) === f(b) })
    }
  }

  test(s"Foldable[$name].reduceLeftOption/reduceRightOption") {
    forAll { (fa: F[Int]) =>
      val list = fa.toList
      assert(fa.reduceLeftOption(_ - _) === (list.reduceLeftOption(_ - _)))
      assert(fa.reduceRightOption((x, ly) => ly.map(x - _)).value === (list.reduceRightOption(_ - _)))
    }
  }

  test(s"Foldable[$name].sumAll") {
    forAll { (fa: F[Int]) =>
      assert(fa.sumAll === (fa.toList.sum))
      assert(fa.sumAll === (iterator(fa).toList.sum))
    }
  }

  test(s"Foldable[$name].productAll") {
    forAll { (fa: F[Int]) =>
      assert(fa.productAll === (fa.toList.product))
      assert(fa.productAll === (iterator(fa).toList.product))
    }
  }

  test(s"Foldable[$name].combineAllOption") {
    forAll { (fa: F[Int]) =>
      assert(fa.combineAllOption === (fa.toList.combineAllOption))
      assert(fa.combineAllOption === (iterator(fa).toList.combineAllOption))
    }
  }

  test(s"Foldable[$name].iterable") {
    forAll { (fa: F[Int]) =>
      assert(fa.toIterable.toList === (fa.toList))
      assert(fa.toIterable.toList === (iterator(fa).toList))
    }
  }

  test(s"Foldable[$name].intercalate") {
    forAll { (fa: F[String], a: String) =>
      assert(fa.intercalate(a) === (fa.toList.mkString(a)))
    }
  }

  test(s"Foldable[$name].toList") {
    forAll { (fa: F[Int]) =>
      assert(fa.toList === (iterator(fa).toList))
    }
  }

  test(s"Foldable[$name] mkString_") {
    forAll { (fa: F[Int]) =>
      assert(fa.mkString_("L[", ";", "]") === (fa.toList.mkString("L[", ";", "]")))
    }
  }

  test(s"Foldable[$name] mkString_ delimiter only") {
    forAll { (fa: F[Int]) =>
      assert(fa.mkString_(",") === (fa.toList.mkString(",")))
    }
  }

  test(s"Foldable[$name].collectFirstSomeM") {
    forAll { (fa: F[Int], n: Int) =>
      assert(
        fa.collectFirstSomeM(x => (x > n).guard[Option].as(x).asRight[String]) ===
          fa.toList
            .collectFirst {
              case x if x > n => x
            }
            .asRight[String]
      )
    }
  }

  test(s"Foldable[$name].sliding2 consistent with List#sliding(2)") {
    forAll { (fi: F[Int]) =>
      val n = 2
      checkSlidingNConsistent(fi, n, Foldable[F].sliding2) { case x1 :: x2 :: Nil =>
        (x1, x2)
      }
    }
  }
  test(s"Foldable[$name].sliding3 consistent with List#sliding(3)") {
    forAll { (fi: F[Int]) =>
      val n = 3
      checkSlidingNConsistent(fi, n, Foldable[F].sliding3) { case x1 :: x2 :: x3 :: Nil =>
        (x1, x2, x3)
      }
    }
  }
  test(s"Foldable[$name].sliding4 consistent with List#sliding(4)") {
    forAll { (fi: F[Int]) =>
      val n = 4
      checkSlidingNConsistent(fi, n, Foldable[F].sliding4) { case x1 :: x2 :: x3 :: x4 :: Nil =>
        (x1, x2, x3, x4)
      }
    }
  }
  test(s"Foldable[$name].sliding5 consistent with List#sliding(5)") {
    forAll { (fi: F[Int]) =>
      val n = 5
      checkSlidingNConsistent(fi, n, Foldable[F].sliding5) { case x1 :: x2 :: x3 :: x4 :: x5 :: Nil =>
        (x1, x2, x3, x4, x5)
      }
    }
  }
  test(s"Foldable[$name].sliding6 consistent with List#sliding(6)") {
    forAll { (fi: F[Int]) =>
      val n = 6
      checkSlidingNConsistent(fi, n, Foldable[F].sliding6) { case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: Nil =>
        (x1, x2, x3, x4, x5, x6)
      }
    }
  }
  test(s"Foldable[$name].sliding7 consistent with List#sliding(7)") {
    forAll { (fi: F[Int]) =>
      val n = 7
      checkSlidingNConsistent(fi, n, Foldable[F].sliding7) { case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: Nil =>
        (x1, x2, x3, x4, x5, x6, x7)
      }
    }
  }
  test(s"Foldable[$name].sliding8 consistent with List#sliding(8)") {
    forAll { (fi: F[Int]) =>
      val n = 8
      checkSlidingNConsistent(fi, n, Foldable[F].sliding8) { case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: Nil =>
        (x1, x2, x3, x4, x5, x6, x7, x8)
      }
    }
  }
  // skip sliding 10-22 as arbitrary collections of that size aren't generated
  test(s"Foldable[$name].sliding9 consistent with List#sliding(9)") {
    forAll { (fi: F[Int]) =>
      val n = 9
      checkSlidingNConsistent(fi, n, Foldable[F].sliding9) {
        case x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: Nil => (x1, x2, x3, x4, x5, x6, x7, x8, x9)
      }
    }
  }

  def checkSlidingNConsistent[Tup <: Product: Eq](fi: F[Int], n: Int, slidingN: F[Int] => List[Tup])(
    pf: PartialFunction[List[Int], Tup]
  ): Unit = {
    val result = slidingN(fi)
    if (n <= fi.size) {
      val expected = fi.toList
        .sliding(n)
        .map(pf)
        .toList
      assert(result === expected)
    } else {
      assert(result.isEmpty)
    }
  }

}

class FoldableSuiteAdditional
    extends CatsSuite
    with ScalaVersionSpecificFoldableSuite
    with FoldableSuiteAdditionalStreamSpecific {

  // exists method written in terms of foldRight
  def contains[F[_]: Foldable, A: Eq](as: F[A], goal: A): Eval[Boolean] =
    as.foldRight(Now(false)) { (a, lb) =>
      if (a === goal) Now(true) else lb
    }

  test("Foldable[List]") {
    val F = Foldable[List]

    // some basic sanity checks
    val ns = (1 to 10).toList
    val total = ns.sum
    assert(F.foldLeft(ns, 0)(_ + _) === total)
    assert(F.foldRight(ns, Now(0))((x, ly) => ly.map(x + _)).value === total)
    assert(F.fold(ns) === total)

    // more basic checks
    val names = List("Aaron", "Betty", "Calvin", "Deirdra")
    assert(F.foldMap(names)(_.length) === (names.map(_.length).sum))
    val sumM = F.foldM(names, "") { (acc, x) =>
      Some(acc + x): Option[String]
    }
    assert(sumM == Some("AaronBettyCalvinDeirdra"))
    val sumMapM = F.foldMapM(names) { x =>
      Some(x): Option[String]
    }
    assert(sumMapM == Some("AaronBettyCalvinDeirdra"))

    // foldMapM should short-circuit and not call the function when not necessary
    val f = (_: String) match {
      case "Calvin" => None
      case "Deirdra" =>
        assert(false)
        None
      case x => Some(x)
    }
    names.foldMapM(f)
    names.foldMapA(f)

    val isNotCalvin: String => Option[String] =
      x => if (x == "Calvin") None else Some(x)
    val notCalvin = F.foldM(names, "") { (acc, x) =>
      isNotCalvin(x).map(acc + _)
    }
    assert(notCalvin == None)
    val notCalvinMapM = F.foldMapM(names)(isNotCalvin)
    assert(notCalvinMapM == None)

    // test trampolining
    val large = (1 to 10000).toList
    assert(contains(large, 10000).value)

    // safely build large lists
    val larger = F.foldRight(large, Now(List.empty[Int]))((x, lxs) => lxs.map((x + 1) :: _))
    assert(larger.value === (large.map(_ + 1)))

    val sum = F.foldRightDefer(large, Eval.later(0))((elem, acc) => acc.map(_ + elem))
    assert(sum.value === (large.sum))

    def boom[A]: Eval[A] = Eval.later(sys.error("boom"))
    // Ensure that the lazy param is actually handled lazily
    val lazySum: Eval[Int] = F.foldRightDefer(large, boom[Int])((elem, acc) => acc.map(_ + elem))
  }

  def checkMonadicFoldsStackSafety[F[_]: Foldable](fromRange: Range => F[Int]): Unit = {
    def nonzero(acc: Long, x: Int): Option[Long] =
      if (x == 0) None else Some(acc + x)

    def gte(lb: Int, x: Int): Option[Boolean] =
      if (x >= lb) Some(true) else Some(false)

    def gteSome(lb: Int, x: Int): Option[Option[Int]] =
      if (x >= lb) Some(Some(x)) else Some(None)

    val n = 100000
    val src = fromRange(1 to n)

    val foldMExpected = n.toLong * (n.toLong + 1) / 2
    val foldMResult = src.foldM(0L)(nonzero)
    assert(foldMResult.get == foldMExpected)

    val existsMExpected = true
    val existsMResult = src.existsM(gte(n, _))
    assert(existsMResult.get == existsMExpected)

    val forallMExpected = true
    val forallMResult = src.forallM(gte(0, _))
    assert(forallMResult.get == forallMExpected)

    val findMExpected = Some(n)
    val findMResult = src.findM(gte(n, _))
    assert(findMResult.get == findMExpected)

    val collectFirstSomeMExpected = Some(n)
    val collectFirstSomeMResult = src.collectFirstSomeM(gteSome(n, _))
    assert(collectFirstSomeMResult.get == collectFirstSomeMExpected)
  }

  def checkSlidingNStackSafety[F[_]: Foldable](fromRange: Range => F[Int]): Unit = {
    val n = 1000
    val src = fromRange(1 to n)

    val sliding2Expected = List.tabulate(n)(i => (i, i + 1)).tail
    val sliding2Result = src.sliding2
    assertEquals(sliding2Result, sliding2Expected)
  }

  test(s"Foldable.iterateRight") {
    forAll { (fa: List[Int]) =>
      val eval = Foldable.iterateRight(fa, Eval.later(0)) { (a, eb) =>
        Eval.always(a + eb.value)
      }

      assert(eval.value === (fa.sum))

      // Repeat here so the result is evaluated again
      assert(eval.value === (fa.sum))
    }
  }

  test("Foldable[List] monadic folds stack safety")(checkMonadicFoldsStackSafety(_.toList))
  test("Foldable[List].slidingN stack safety")(checkSlidingNStackSafety(_.toList))

  test("Foldable[Vector] monadic folds stack safety")(checkMonadicFoldsStackSafety(_.toVector))
  test("Foldable[Vector].slidingN stack safety")(checkSlidingNStackSafety(_.toVector))

  test("Foldable[SortedSet] monadic folds stack safety")(checkMonadicFoldsStackSafety(xs => SortedSet(xs: _*)))
  test("Foldable[SortedSet].slidingN stack safety")(checkSlidingNStackSafety(xs => SortedSet(xs: _*)))

  // Can't checkSlidingNStackSafety because of iteration order.
  test("Foldable[SortedMap[String, *]] monadic stack safety") {
    checkMonadicFoldsStackSafety(xs => SortedMap(xs.map(x => x.toString -> x): _*))
  }

  test("Foldable[NonEmptyList] monadic folds stack safety")(
    checkMonadicFoldsStackSafety(xs => NonEmptyList.fromListUnsafe(xs.toList))
  )

  test("Foldable[NonEmptyList].slidingN stack safety")(
    checkSlidingNStackSafety(xs => NonEmptyList.fromListUnsafe(xs.toList))
  )

  test("Foldable[NonEmptyVector] monadic folds stack safety")(
    checkMonadicFoldsStackSafety(xs => NonEmptyVector.fromVectorUnsafe(xs.toVector))
  )

  test("Foldable[NonEmptyVector].slidingN stack safety")(
    checkSlidingNStackSafety(xs => NonEmptyVector.fromVectorUnsafe(xs.toVector))
  )

  test("Foldable[List] doesn't break substitution") {
    val result = List.range(0, 10).foldM(List.empty[Int])((accum, elt) => Eval.always(elt :: accum))

    assert(result.value == result.value)
  }
}

// `Stream` is deprecated since Scala 2.13 therefore all tests involving this type are
// gathered here to suppress deprecation warnings at once.
@annotation.nowarn("cat=deprecation")
sealed trait FoldableSuiteAdditionalStreamSpecific { self: FoldableSuiteAdditional =>
  test("Foldable[Stream] monadic folds stack safety")(checkMonadicFoldsStackSafety(_.toStream))
  test("Foldable[Stream].slidingN stack safety")(checkSlidingNStackSafety(_.toStream))

  test("Foldable[NonEmptyStream] monadic folds stack safety")(
    checkMonadicFoldsStackSafety(xs => NonEmptyStream(xs.head, xs.tail: _*))
  )

  test("Foldable[NonEmptyStream].slidingN stack safety")(
    checkSlidingNStackSafety(xs => NonEmptyStream(xs.head, xs.tail: _*))
  )

  val F = Foldable[Stream]
  def bomb[A]: A = sys.error("boom")
  val dangerous = 0 #:: 1 #:: 2 #:: bomb[Int] #:: Stream.empty
  def boom[A]: Stream[A] = bomb[A] #:: Stream.empty

  test("Foldable[Stream] doesn't blow up") {

    // doesn't blow up - this also ensures it works for infinite streams.
    assert(contains(dangerous, 2).value)
  }

  test("Foldable[Stream] lazy results don't blow up unless you call .value on them") {
    contains(dangerous, -1)
  }

  test("Foldable[Stream] param to foldRight is actually being handled lazily") {
    // ensure that the . it only needs to be evaluated if we reach the
    // "end" of the fold.
    val trap = Eval.later(bomb[Boolean])
    val result = F.foldRight(1 #:: 2 #:: Stream.empty, trap) { (n, lb) =>
      if (n == 2) Now(true) else lb
    }
    assert(result.value)
  }

  test("Foldable[Stream]  trampolining") {
    val large = Stream((1 to 10000): _*)
    assert(contains(large, 10000).value)
  }

  test("Foldable[Stream] laziness of foldM") {
    assert(dangerous.foldM(0)((acc, a) => if (a < 2) Some(acc + a) else None) === None)
  }

  def foldableStreamWithDefaultImpl: Foldable[Stream] =
    new Foldable[Stream] {
      def foldLeft[A, B](fa: Stream[A], b: B)(f: (B, A) => B): B =
        Foldable[Stream].foldLeft(fa, b)(f)

      def foldRight[A, B](fa: Stream[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable[Stream].foldRight(fa, lb)(f)
    }

  test(".foldA successful case") {
    implicit val F: Foldable[Stream] = foldableStreamWithDefaultImpl
    val ns = Stream.apply[Either[String, Int]](1.asRight, 2.asRight, 7.asRight)

    assert(F.foldA(ns) == 10.asRight[String])
  }

  test(".foldA failed case") {
    implicit val F: Foldable[Stream] = foldableStreamWithDefaultImpl
    val ns = Stream.apply[Either[String, Int]](1.asRight, "boom!!!".asLeft, 7.asRight)

    assert(ns.foldA == "boom!!!".asLeft[Int])
  }

  test(".foldA short-circuiting") {
    implicit val F: Foldable[Stream] = foldableStreamWithDefaultImpl
    val ns = Stream.from(1).map(n => if (n >= 100000) Left(n) else Right(n))

    assert(F.foldA(ns) === Left(100000))
  }

  test(".foldLeftM short-circuiting") {
    implicit val F: Foldable[Stream] = foldableStreamWithDefaultImpl
    val ns = Stream.continually(1)
    val res = F.foldLeftM[Either[Int, *], Int, Int](ns, 0) { (sum, n) =>
      if (sum >= 100000) Left(sum) else Right(sum + n)
    }
    assert(res == Left(100000))
  }

  test(".foldLeftM short-circuiting optimality") {
    implicit val F: Foldable[Stream] = foldableStreamWithDefaultImpl

    // test that no more elements are evaluated than absolutely necessary

    def concatUntil(ss: Stream[String], stop: String): Either[String, String] =
      F.foldLeftM[Either[String, *], String, String](ss, "") { (acc, s) =>
        if (s == stop) Left(acc) else Right(acc + s)
      }

    assert(concatUntil("STOP" #:: boom[String], "STOP") == Left(""))
    assert(concatUntil("Zero" #:: "STOP" #:: boom[String], "STOP") == Left("Zero"))
    assert(concatUntil("Zero" #:: "One" #:: "STOP" #:: boom[String], "STOP") == Left("ZeroOne"))
  }

  test(".existsM/.forallM short-circuiting") {
    implicit val F: Foldable[Stream] = foldableStreamWithDefaultImpl
    assert(F.existsM[Id, Boolean](true #:: boom[Boolean])(identity) == true)
    assert(F.forallM[Id, Boolean](false #:: boom[Boolean])(identity) == false)
  }

  test(".findM/.collectFirstSomeM short-circuiting") {
    implicit val F: Foldable[Stream] = foldableStreamWithDefaultImpl
    assert((1 #:: boom[Int]).findM[Id](_ > 0) == Some(1))
    assert((1 #:: boom[Int]).collectFirstSomeM[Id, Int](Option.apply) == Some(1))
  }
}

class FoldableListSuite extends FoldableSuite[List]("list") {
  def iterator[T](list: List[T]): Iterator[T] = list.iterator
}

class FoldableVectorSuite extends FoldableSuite[Vector]("vector") {
  def iterator[T](vector: Vector[T]): Iterator[T] = vector.iterator
}

class FoldableSortedSetSuite extends FoldableSuite[SortedSet]("sortedSet") {
  def iterator[T](set: SortedSet[T]): Iterator[T] = set.iterator
}

@annotation.nowarn("cat=deprecation")
class FoldableStreamSuite extends FoldableSuite[Stream]("stream") {
  def iterator[T](list: Stream[T]): Iterator[T] = list.iterator
}

class FoldableSortedMapSuite extends FoldableSuite[SortedMap[Int, *]]("sortedMap") {
  def iterator[T](map: SortedMap[Int, T]): Iterator[T] = map.valuesIterator
}

class FoldableOptionSuite extends FoldableSuite[Option]("option") {
  def iterator[T](option: Option[T]): Iterator[T] = option.iterator
}

class FoldableEitherSuite extends FoldableSuite[Either[Int, *]]("either") {
  def iterator[T](either: Either[Int, T]): Iterator[T] = either.toOption.iterator
}

class FoldableValidatedSuite extends FoldableSuite[Validated[String, *]]("validated") {
  def iterator[T](validated: Validated[String, T]): Iterator[T] = validated.toOption.iterator
}

class FoldableTrySuite extends FoldableSuite[Try]("try") {
  def iterator[T](tryt: Try[T]): Iterator[T] = tryt.toOption.iterator
}

class FoldableEitherKSuite extends FoldableSuite[EitherK[Option, Option, *]]("eitherK") {
  def iterator[T](eitherK: EitherK[Option, Option, T]) = eitherK.run.bimap(_.iterator, _.iterator).merge
}

class FoldableIorSuite extends FoldableSuite[Ior[Int, *]]("ior") {
  def iterator[T](ior: Int Ior T) =
    ior.fold(_ => None.iterator, b => Some(b).iterator, (_, b) => Some(b).iterator)
}

class FoldableIdSuite extends FoldableSuite[Id[*]]("id") {
  def iterator[T](id: Id[T]) = Some(id).iterator
}

class FoldableIdTSuite extends FoldableSuite[IdT[Option, *]]("idT") {
  def iterator[T](idT: IdT[Option, T]) = idT.value.iterator
}

class FoldableConstSuite extends FoldableSuite[Const[Int, *]]("const") {
  def iterator[T](const: Const[Int, T]): Iterator[T] = None.iterator
}

class FoldableTuple2Suite extends FoldableSuite[(Int, *)]("tuple2") {
  def iterator[T](tuple: (Int, T)) = Some(tuple._2).iterator
}

class FoldableOneAndSuite extends FoldableSuite[OneAnd[List, *]]("oneAnd") {
  def iterator[T](oneAnd: OneAnd[List, T]) = (oneAnd.head :: oneAnd.tail).iterator
}

class FoldableComposedSuite extends FoldableSuite[Nested[List, Option, *]]("nested") {
  def iterator[T](nested: Nested[List, Option, T]) =
    nested.value.collect { case Some(t) =>
      t
    }.iterator
}
