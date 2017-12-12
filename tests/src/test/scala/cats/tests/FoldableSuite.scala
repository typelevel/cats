package cats
package tests

import org.scalatest.prop.PropertyChecks
import org.scalacheck.Arbitrary
import scala.util.Try
import scala.collection.immutable._
import cats.instances.all._
import cats.data._
import cats.laws.discipline.arbitrary._

abstract class FoldableSuite[F[_]: Foldable](name: String)(
  implicit ArbFInt: Arbitrary[F[Int]],
     ArbFString: Arbitrary[F[String]]) extends CatsSuite with PropertyChecks {

  def iterator[T](fa: F[T]): Iterator[T]

  test(s"Foldable[$name].size/get") {
    forAll { (fa: F[Int], n: Int) =>
      val s = fa.size
      s should === (iterator(fa).size.toLong)
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
      (lefts <+> rights).size.toLong should === (fi.size)
    }
  }

  test("Foldable#partitionEither consistent with List#partition") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val list = Foldable[F].toList(fi)
      val (lefts, rights) = Foldable[List].partitionEither(list)(f)
      val (ls, rs) = list.map(f).partition({
        case Left(_) => true
        case Right(_) => false
      })

      lefts.map(_.asLeft[String]) should === (ls)
      rights.map(_.asRight[String]) should === (rs)
    }
  }

  test("Foldable#partitionEither to one side is identity") {
    forAll { (fi: F[Int], f: Int => String) =>
      val list = Foldable[F].toList(fi)
      val g: Int => Either[Double, String] = f andThen Right.apply
      val h: Int => Either[String, Double] = f andThen Left.apply

      val withG = Foldable[List].partitionEither(list)(g)._2
      withG should === (list.map(f))

      val withH = Foldable[List].partitionEither(list)(h)._1
      withH should === (list.map(f))
    }
  }

  test("Foldable#partitionEither remains sorted") {
    forAll { (fi: F[Int], f: Int => Either[String, String]) =>
      val list = Foldable[F].toList(fi)

      val sorted = list.map(f).sorted
      val (lefts, rights) = Foldable[List].partitionEither(sorted)(identity)

      lefts.sorted should === (lefts)
      rights.sorted should === (rights)
    }
  }

  test(s"Foldable[$name] summation") {
    forAll { (fa: F[Int]) =>
      val total = iterator(fa).sum
      fa.foldLeft(0)(_ + _) should === (total)
      fa.foldRight(Now(0))((x, ly) => ly.map(x + _)).value should === (total)
      fa.fold should === (total)
      fa.foldMap(identity) should === (total)
    }
  }

  test(s"Foldable[$name].find/exists/forall/existsM/forallM/filter_/dropWhile_") {
    forAll { (fa: F[Int], n: Int) =>
      fa.find(_ > n)   should === (iterator(fa).find(_ > n))
      fa.exists(_ > n) should === (iterator(fa).exists(_ > n))
      fa.forall(_ > n) should === (iterator(fa).forall(_ > n))
      fa.existsM(k => Option(k > n)) should === (Option(iterator(fa).exists(_ > n)))
      fa.forallM(k => Option(k > n)) should === (Option(iterator(fa).forall(_ > n)))
      fa.filter_(_ > n) should === (iterator(fa).filter(_ > n).toList)
      fa.dropWhile_(_ > n) should === (iterator(fa).dropWhile(_ > n).toList)
      fa.takeWhile_(_ > n) should === (iterator(fa).takeWhile(_ > n).toList)
    }
  }

  test(s"Foldable[$name].toList/isEmpty/nonEmpty") {
    forAll { (fa: F[Int]) =>
      fa.toList should === (iterator(fa).toList)
      fa.isEmpty should === (iterator(fa).isEmpty)
      fa.nonEmpty should === (iterator(fa).nonEmpty)
    }
  }

  test(s"Foldable[$name].maximum/minimum") {
    forAll { (fa: F[Int]) =>
      val maxOpt = fa.maximumOption
      val minOpt = fa.minimumOption
      val list = fa.toList
      val nelOpt = list.toNel
      maxOpt should === (nelOpt.map(_.maximum))
      maxOpt should === (nelOpt.map(_.toList.max))
      minOpt should === (nelOpt.map(_.minimum))
      minOpt should === (nelOpt.map(_.toList.min))
      maxOpt.forall(i => fa.forall(_ <= i)) should === (true)
      minOpt.forall(i => fa.forall(_ >= i)) should === (true)
    }
  }

  test(s"Foldable[$name].reduceLeftOption/reduceRightOption") {
    forAll { (fa: F[Int]) =>
      val list = fa.toList
      fa.reduceLeftOption(_ - _) should === (list.reduceLeftOption(_ - _))
      fa.reduceRightOption((x, ly) => ly.map(x - _)).value should === (list.reduceRightOption(_ - _))
    }
  }

  test(s"Foldable[$name].intercalate") {
    forAll { (fa: F[String], a: String) =>
      fa.intercalate(a) should === (fa.toList.mkString(a))
    }
  }

  test(s"Foldable[$name].toList") {
    forAll { (fa: F[Int]) =>
      fa.toList should === (iterator(fa).toList)
    }
  }
}

class FoldableSuiteAdditional extends CatsSuite {

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
    F.foldLeft(ns, 0)(_ + _) should === (total)
    F.foldRight(ns, Now(0))((x, ly) => ly.map(x + _)).value should === (total)
    F.fold(ns) should === (total)

    // more basic checks
    val names = List("Aaron", "Betty", "Calvin", "Deirdra")
    F.foldMap(names)(_.length) should === (names.map(_.length).sum)
    val sumM = F.foldM(names, "") { (acc, x) => (Some(acc + x): Option[String]) }
    assert(sumM == Some("AaronBettyCalvinDeirdra"))
    val sumMapM = F.foldMapM(names) { x => (Some(x): Option[String]) }
    assert(sumMapM == Some("AaronBettyCalvinDeirdra"))
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
    larger.value should === (large.map(_ + 1))
  }

  def checkFoldMStackSafety[F[_]](fromRange: Range => F[Int])(implicit F: Foldable[F]): Unit = {
    def nonzero(acc: Long, x: Int): Option[Long] =
      if (x == 0) None else Some(acc + x)

    val n = 100000
    val expected = n.toLong*(n.toLong+1)/2
    val foldMResult = F.foldM(fromRange(1 to n), 0L)(nonzero)
    assert(foldMResult.get == expected)
    ()
  }
  test(s"Foldable.iterateRight") {
    forAll { (fa: List[Int]) =>
      val eval = Foldable.iterateRight(fa, Eval.later(0)) { (a, eb) =>
        Eval.always(a + eb.value)
      }

      eval.value should === (fa.sum)

      //Repeat here so the result is evaluated again
      eval.value should === (fa.sum)
    }
  }

  test("Foldable[List].foldM stack safety") {
    checkFoldMStackSafety[List](_.toList)
  }

  test("Foldable[Stream].foldM stack safety") {
    checkFoldMStackSafety[Stream](_.toStream)
  }

  test("Foldable[Vector].foldM stack safety") {
    checkFoldMStackSafety[Vector](_.toVector)
  }

  test("Foldable[SortedSet].foldM stack safety") {
    checkFoldMStackSafety[SortedSet](_.to)
  }

  test("Foldable[SortedMap[String, ?]].foldM stack safety") {
    checkFoldMStackSafety[SortedMap[String, ?]](xs => SortedMap.empty[String, Int] ++ xs.map(x => x.toString -> x).toMap)
  }

  test("Foldable[NonEmptyList].foldM stack safety") {
    checkFoldMStackSafety[NonEmptyList](xs => NonEmptyList.fromListUnsafe(xs.toList))
  }

  test("Foldable[NonEmptyVector].foldM stack safety") {
    checkFoldMStackSafety[NonEmptyVector](xs => NonEmptyVector.fromVectorUnsafe(xs.toVector))
  }

  test("Foldable[NonEmptyStream].foldM stack safety") {
    checkFoldMStackSafety[NonEmptyStream](xs => NonEmptyStream(xs.head, xs.tail: _*))
  }

  test("Foldable[Stream]") {
    val F = Foldable[Stream]

    def bomb[A]: A = sys.error("boom")
    val dangerous = 0 #:: 1 #:: 2 #:: bomb[Stream[Int]]

    // doesn't blow up - this also ensures it works for infinite streams.
    assert(contains(dangerous, 2).value)

    // lazy results don't blow up unless you call .value on them.
    val doom: Eval[Boolean] = contains(dangerous, -1)

    // ensure that the Lazy[B] param to foldRight is actually being
    // handled lazily. it only needs to be evaluated if we reach the
    // "end" of the fold.
    val trap = Eval.later(bomb[Boolean])
    val result = F.foldRight(1 #:: 2 #:: Stream.empty, trap) { (n, lb) =>
      if (n == 2) Now(true) else lb
    }
    assert(result.value)

    // test trampolining
    val large = Stream((1 to 10000): _*)
    assert(contains(large, 10000).value)

    // test laziness of foldM
    dangerous.foldM(0)((acc, a) => if (a < 2) Some(acc + a) else None) should === (None)

  }

  def foldableStreamWithDefaultImpl = new Foldable[Stream] {
    def foldLeft[A, B](fa: Stream[A], b: B)(f: (B, A) => B): B =
      instances.stream.catsStdInstancesForStream.foldLeft(fa, b)(f)

    def foldRight[A, B](fa: Stream[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      instances.stream.catsStdInstancesForStream.foldRight(fa, lb)(f)
  }

  test(".foldLeftM short-circuiting") {
    implicit val F = foldableStreamWithDefaultImpl
    val ns = Stream.continually(1)
    val res = F.foldLeftM[Either[Int, ?], Int, Int](ns, 0) { (sum, n) =>
      if (sum >= 100000) Left(sum) else Right(sum + n)
    }
    assert(res == Left(100000))
  }

  test(".foldLeftM short-circuiting optimality") {
    implicit val F = foldableStreamWithDefaultImpl

    // test that no more elements are evaluated than absolutely necessary

    def concatUntil(ss: Stream[String], stop: String): Either[String, String] =
      F.foldLeftM[Either[String, ?], String, String](ss, "") { (acc, s) =>
        if (s == stop) Left(acc) else Right(acc + s)
      }

    def boom: Stream[String] = sys.error("boom")
    assert(concatUntil("STOP" #:: boom, "STOP") == Left(""))
    assert(concatUntil("Zero" #:: "STOP" #:: boom, "STOP") == Left("Zero"))
    assert(concatUntil("Zero" #:: "One" #:: "STOP" #:: boom, "STOP") == Left("ZeroOne"))
  }

  test(".existsM/.forallM short-circuiting") {
    implicit val F = foldableStreamWithDefaultImpl
    def boom: Stream[Boolean] = sys.error("boom")
    assert(F.existsM[Id, Boolean](true #:: boom)(identity) == true)
    assert(F.forallM[Id, Boolean](false #:: boom)(identity) == false)
  }

  test("Foldable[List] doesn't break substitution") {
    val result  = List.range(0,10).foldM(List.empty[Int])((accum, elt) => Eval.always(elt :: accum))

    assert(result.value == result.value)
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

class FoldableStreamSuite extends FoldableSuite[Stream]("stream") {
  def iterator[T](stream: Stream[T]): Iterator[T] = stream.iterator
}

class FoldableSortedMapSuite extends FoldableSuite[SortedMap[Int, ?]]("sortedMap") {
  def iterator[T](map: SortedMap[Int, T]): Iterator[T] = map.valuesIterator
}

class FoldableOptionSuite extends FoldableSuite[Option]("option") {
  def iterator[T](option: Option[T]): Iterator[T] = option.iterator
}

class FoldableEitherSuite extends FoldableSuite[Either[Int, ?]]("either") {
  def iterator[T](either: Either[Int, T]): Iterator[T] = either.right.toOption.iterator
}

class FoldableValidatedSuite extends FoldableSuite[Validated[String, ?]]("validated") {
  def iterator[T](validated: Validated[String, T]): Iterator[T] = validated.toOption.iterator
}

class FoldableTrySuite extends FoldableSuite[Try]("try") {
  def iterator[T](tryt: Try[T]): Iterator[T] = tryt.toOption.iterator
}

class FoldableEitherKSuite extends FoldableSuite[EitherK[Option, Option, ?]]("eitherK") {
  def iterator[T](eitherK: EitherK[Option, Option, T]) = eitherK.run.bimap(_.iterator, _.iterator).merge
}

class FoldableIorSuite extends FoldableSuite[Int Ior ?]("ior") {
  def iterator[T](ior: Int Ior T) =
    ior.fold(_ => None.iterator, b => Some(b).iterator, (_, b) => Some(b).iterator)
}

class FoldableIdSuite extends FoldableSuite[Id[?]]("id") {
  def iterator[T](id: Id[T]) = Some(id).iterator
}

class FoldableIdTSuite extends FoldableSuite[IdT[Option, ?]]("idT") {
  def iterator[T](idT: IdT[Option, T]) = idT.value.iterator
}

class FoldableConstSuite extends FoldableSuite[Const[Int, ?]]("const") {
  def iterator[T](const: Const[Int, T]) = None.iterator
}

class FoldableTuple2Suite extends FoldableSuite[(Int, ?)]("tuple2") {
  def iterator[T](tuple: (Int, T)) = Some(tuple._2).iterator
}

class FoldableOneAndSuite extends FoldableSuite[OneAnd[List, ?]]("oneAnd") {
  def iterator[T](oneAnd: OneAnd[List, T]) = (oneAnd.head :: oneAnd.tail).iterator
}

class FoldableComposedSuite extends FoldableSuite[Nested[List, Option, ?]]("nested") {
  def iterator[T](nested: Nested[List, Option, T]) = nested.value.collect {
    case Some(t) => t
  }.iterator
}
