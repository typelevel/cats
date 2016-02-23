package cats
package tests

import algebra.laws.OrderLaws

import cats.data.{Streaming, StreamingT}
import cats.laws.discipline.{CartesianTests, CoflatMapTests, MonadCombineTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class StreamingTTests extends CatsSuite {

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[StreamingT[Eval, ?]]
    checkAll("StreamingT[Eval, ?]", MonadCombineTests[StreamingT[Eval, ?]].monadCombine[Int, Int, Int])
    checkAll("StreamingT[Eval, ?]", CoflatMapTests[StreamingT[Eval, ?]].coflatMap[Int, Int, Int])
    checkAll("StreamingT[Eval, Int]", OrderLaws[StreamingT[Eval, Int]].order)
    checkAll("Monad[StreamingT[Eval, ?]]", SerializableTests.serializable(Monad[StreamingT[Eval, ?]]))
  }

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[StreamingT[Option, ?]]
    checkAll("StreamingT[Option, ?]", MonadCombineTests[StreamingT[Option, ?]].monadCombine[Int, Int, Int])
    checkAll("StreamingT[Option, ?]", CoflatMapTests[StreamingT[Option, ?]].coflatMap[Int, Int, Int])
    checkAll("StreamingT[Option, Int]", OrderLaws[StreamingT[Option, Int]].order)
    checkAll("Monad[StreamingT[Option, ?]]", SerializableTests.serializable(Monad[StreamingT[Option, ?]]))
  }

  {
    implicit val iso = CartesianTests.Isomorphisms.invariant[StreamingT[List, ?]]
    checkAll("StreamingT[List, ?]", MonadCombineTests[StreamingT[List, ?]].monadCombine[Int, Int, Int])
    checkAll("StreamingT[List, ?]", CoflatMapTests[StreamingT[List, ?]].coflatMap[Int, Int, Int])
    checkAll("StreamingT[List, Int]", OrderLaws[StreamingT[List, Int]].order)
    checkAll("Monad[StreamingT[List, ?]]", SerializableTests.serializable(Monad[StreamingT[List, ?]]))
  }

  {
    implicit val F = ListWrapper.monad
    implicit val O = ListWrapper.partialOrder[List[Int]]
    checkAll("StreamingT[ListWrapper, Int]", OrderLaws[StreamingT[ListWrapper, Int]].partialOrder)
    checkAll("PartialOrder[StreamingT[ListWrapper, Int]]", SerializableTests.serializable(PartialOrder[StreamingT[ListWrapper, Int]]))
  }

  {
    implicit val F = ListWrapper.monad
    implicit val E = ListWrapper.eqv[List[Int]]
    checkAll("StreamingT[ListWrapper, Int]", OrderLaws[StreamingT[ListWrapper, Int]].eqv)
    checkAll("Eq[StreamingT[ListWrapper, Int]]", SerializableTests.serializable(Eq[StreamingT[ListWrapper, Int]]))
  }


  test("uncons with Id consistent with List headOption/tail") {
    forAll { (s: StreamingT[Id, Int]) =>
      val sList = s.toList
      s.uncons.map{ case (h, t) =>
        (h, t.toList)
      } should === (sList.headOption.map{ h =>
        (h, sList.tail)
      })
    }
  }

  test("map with Id consistent with List.map") {
    forAll { (s: StreamingT[Id, Int], f: Int => Long) =>
      s.map(f).toList should === (s.toList.map(f))
    }
  }

  test("flatMap with Id consistent with List.flatMap") {
    forAll { (s: StreamingT[Id, Int], f: Int => StreamingT[Id, Long]) =>
      s.flatMap(f).toList should === (s.toList.flatMap(f(_).toList))
    }
  }

  test("filter with Id consistent with List.filter") {
    forAll { (s: StreamingT[Id, Int], f: Int => Boolean) =>
      s.filter(f).toList should === (s.toList.filter(f))
    }
  }

  test("filter - check regression") {
    val s = StreamingT[Option, Int](1, 2, 1)
    s.filter(_ > 1).toList should === (Some(List(2)))
  }

  test("foldLeft with Id consistent with List.foldLeft") {
    forAll { (s: StreamingT[Id, Int], l: Long, f: (Long, Int) => Long) =>
      s.foldLeft(l)(f) should === (s.toList.foldLeft(l)(f))
    }
  }

  test("find with Id consistent with List.find") {
    forAll { (s: StreamingT[Id, Int], f: Int => Boolean) =>
      s.find(f) should === (s.toList.find(f))
    }
  }

  test("isEmpty with Id consistent with List.isEmpty") {
    forAll { (s: StreamingT[Id, Int]) =>
      s.isEmpty should === (s.toList.isEmpty)
    }
  }

  test("nonEmpty with Id consistent with List.nonEmpty") {
    forAll { (s: StreamingT[Id, Int]) =>
      s.nonEmpty should === (s.toList.nonEmpty)
    }
  }

  test("%:: with Id consistent with List.::") {
    forAll { (i: Int, s: StreamingT[Id, Int]) =>
      (i %:: s).toList should === (i :: s.toList)
    }
  }

  test("%::: with Id consistent with List.:::") {
    forAll { (s1: StreamingT[Id, Int], s2: StreamingT[Id, Int]) =>
      (s1 %::: s2).toList should === (s1.toList ::: s2.toList)
    }
  }

  test("concat with Id consistent with List.++") {
    forAll { (s1: StreamingT[Id, Int], s2: StreamingT[Id, Int]) =>
      (s1 concat s2).toList should === (s1.toList ++ s2.toList)
    }
  }

  test("exists with Id consistent with List.exists") {
    forAll { (s: StreamingT[Id, Int], f: Int => Boolean) =>
      s.exists(f) should === (s.toList.exists(f))
    }
  }

  test("forall with Id consistent with List.forall") {
    forAll { (s: StreamingT[Id, Int], f: Int => Boolean) =>
      s.forall(f) should === (s.toList.forall(f))
    }
  }

  test("takeWhile with Id consistent with List.takeWhile") {
    forAll { (s: StreamingT[Id, Int], f: Int => Boolean) =>
      s.takeWhile(f).toList should === (s.toList.takeWhile(f))
    }
  }

  test("dropWhile with Id consistent with List.dropWhile") {
    forAll { (s: StreamingT[Id, Int], f: Int => Boolean) =>
      s.dropWhile(f).toList should === (s.toList.dropWhile(f))
    }
  }

  test("take with Id consistent with List.take") {
    forAll { (s: StreamingT[Id, Int], i: Int) =>
      s.take(i).toList should === (s.toList.take(i))
    }
  }

  test("drop with Id consistent with List.drop") {
    forAll { (s: StreamingT[Id, Int], i: Int) =>
      s.drop(i).toList should === (s.toList.drop(i))
    }
  }

  test("unfold with Id consistent with Streaming.unfold") {
    forAll { (o: Option[Long]) =>
      val f: Long => Option[Long] = { x  =>
        val rng = new scala.util.Random(x)
        if (rng.nextBoolean) Some(rng.nextLong)
        else None
      }

      StreamingT.unfold[Id, Long](o)(f).toList should === (Streaming.unfold(o)(f).toList)
    }
  }

  test("defer produces the same values") {
    forAll { (xs: StreamingT[Option, Int]) =>
      StreamingT.defer(xs) should === (xs)
    }
  }

  test("defer isn't eager if the pureEval impl isn't") {
    def s: StreamingT[Eval, Int] = throw new RuntimeException("blargh")
    val x = StreamingT.defer[Eval, Int](s)
  }

  test("fromVector") {
    forAll { (xs: Vector[Int]) =>
      StreamingT.fromVector[Id, Int](xs).toList.toVector should === (xs)
    }
  }

  test("fromList") {
    forAll { (xs: List[Int]) =>
      StreamingT.fromList[Id, Int](xs).toList should === (xs)
    }
  }

  test("single consistent with apply") {
    forAll { (i: Int) =>
      StreamingT[Id, Int](i) should === (StreamingT.single[Id, Int](i))
    }
  }

  test("var-arg apply") {
    forAll { (x1: Int, x2: Int, x3: Int, x4: Int) =>
      val fromList = StreamingT.fromList[Id, Int](x1 :: x2 :: x3 :: x4 :: Nil)
      StreamingT[Id, Int](x1, x2, x3, x4) should === (fromList)
    }

    forAll { (x1: Int, x2: Int, tail: List[Int]) =>
      val fromList = StreamingT.fromList[Id, Int](x1 :: x2 :: tail)
      StreamingT[Id, Int](x1, x2, tail: _*) should === (fromList)
    }
  }

  test("toString is wrapped in StreamingT()"){
    forAll { (xs: StreamingT[Option, Int]) =>
      val s = xs.toString
      s.take(11) should === ("StreamingT(")
      s.last should === (')')
    }
  }
}

class SpecificStreamingTTests extends CatsSuite {

  type S[A] = StreamingT[List, A]

  def cons[A](a: A, fs: List[S[A]]): S[A] = StreamingT.cons(a, fs)
  def wait[A](fs: List[S[A]]): S[A] = StreamingT.wait(fs)
  def empty[A]: S[A] = StreamingT.empty[List, A]

  test("counter-example #1"){
    val fa: S[Boolean] =
      cons(true, List(cons(true, List(empty)), empty))

    def f(b: Boolean): S[Boolean] =
      if (b) cons(false, List(cons(true, List(empty))))
      else empty

    def g(b: Boolean): S[Boolean] =
      if (b) empty
      else cons(true, List(cons(false, List(empty)), cons(true, List(empty))))

    val x = fa.flatMap(f).flatMap(g)
    val y = fa.flatMap(a => f(a).flatMap(g))
    x should === (y)
  }
}
