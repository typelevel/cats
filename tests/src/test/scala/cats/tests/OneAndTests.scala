package cats
package tests

import cats.kernel.laws.{GroupLaws, OrderLaws}

import cats.data.{NonEmptyStream, OneAnd}
import cats.laws.discipline.{ComonadTests, FunctorTests, SemigroupKTests, FoldableTests, MonadTests, SerializableTests, CartesianTests, TraverseTests, ReducibleTests}
import cats.laws.discipline.arbitrary._

class OneAndTests extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  checkAll("OneAnd[Stream, Int]", OrderLaws[OneAnd[Stream, Int]].eqv)

  checkAll("OneAnd[Stream, Int] with Option", TraverseTests[OneAnd[Stream, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[OneAnd[Stream, A]]", SerializableTests.serializable(Traverse[OneAnd[Stream, ?]]))

  checkAll("OneAnd[Stream, Int]", ReducibleTests[OneAnd[Stream, ?]].reducible[Option, Int, Int])
  checkAll("Reducible[OneAnd[Stream, ?]]", SerializableTests.serializable(Reducible[OneAnd[Stream, ?]]))

  implicit val iso = CartesianTests.Isomorphisms.invariant[OneAnd[ListWrapper, ?]](OneAnd.catsDataFunctorForOneAnd(ListWrapper.functor))

  // Test instances that have more general constraints
  {
    implicit val monadCombine = ListWrapper.monadCombine
    checkAll("OneAnd[ListWrapper, Int]", CartesianTests[OneAnd[ListWrapper, ?]].cartesian[Int, Int, Int])
    checkAll("Cartesian[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Cartesian[OneAnd[ListWrapper, ?]]))
  }

  {
    implicit val functor = ListWrapper.functor
    checkAll("OneAnd[ListWrapper, Int]", FunctorTests[OneAnd[ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Functor[OneAnd[ListWrapper, ?]]))
  }

  {
    implicit val monadCombine = ListWrapper.monadCombine
    checkAll("OneAnd[ListWrapper, Int]", SemigroupKTests[OneAnd[ListWrapper, ?]].semigroupK[Int])
    checkAll("OneAnd[Stream, Int]", GroupLaws[OneAnd[Stream, Int]].semigroup)
    checkAll("SemigroupK[OneAnd[ListWrapper, A]]", SerializableTests.serializable(SemigroupK[OneAnd[ListWrapper, ?]]))
    checkAll("Semigroup[NonEmptyStream[Int]]", SerializableTests.serializable(Semigroup[OneAnd[Stream, Int]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("OneAnd[ListWrapper, Int]", FoldableTests[OneAnd[ListWrapper, ?]].foldable[Int, Int])
    checkAll("Foldable[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Foldable[OneAnd[ListWrapper, ?]]))
  }

  {
    // Test functor and subclasses don't have implicit conflicts
    implicitly[Functor[NonEmptyStream]]
    implicitly[Monad[NonEmptyStream]]
    implicitly[Comonad[NonEmptyStream]]
  }

  implicit val iso2 = CartesianTests.Isomorphisms.invariant[OneAnd[Stream, ?]]

  checkAll("NonEmptyStream[Int]", MonadTests[NonEmptyStream].monad[Int, Int, Int])
  checkAll("Monad[NonEmptyStream[A]]", SerializableTests.serializable(Monad[NonEmptyStream]))

  checkAll("NonEmptyStream[Int]", ComonadTests[NonEmptyStream].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyStream[A]]", SerializableTests.serializable(Comonad[NonEmptyStream]))

  test("size is consistent with toList.size") {
    forAll { (oa: OneAnd[Vector, Int]) =>
      oa.size should === (oa.toList.size.toLong)
    }
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nel: NonEmptyStream[Int]) =>
      nel.show.nonEmpty should === (true)
      nel.show.startsWith("OneAnd(") should === (true)
      nel.show should === (implicitly[Show[NonEmptyStream[Int]]].show(nel))
      nel.show.contains(nel.head.show) should === (true)
    }
  }

  test("Show is formatted correctly") {
    val oneAnd = NonEmptyStream("Test")
    oneAnd.show should === ("OneAnd(Test, Stream())")
  }

  test("Creating OneAnd + unwrap is identity") {
    forAll { (i: Int, tail: Stream[Int]) =>
      val stream = i #:: tail
      val oneAnd = NonEmptyStream(i, tail: _*)
      stream should === (oneAnd.unwrap)
    }
  }

  test("NonEmptyStream#filter is consistent with Stream#filter") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      nel.filter(p) should === (stream.filter(p))
    }
  }

  test("NonEmptyStream#find is consistent with Stream#find") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      nel.find(p) should === (stream.find(p))
    }
  }

  test("NonEmptyStream#exists is consistent with Stream#exists") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      nel.exists(p) should === (stream.exists(p))
    }
  }

  test("NonEmptyStream#forall is consistent with Stream#forall") {
    forAll { (nel: NonEmptyStream[Int], p: Int => Boolean) =>
      val stream = nel.unwrap
      nel.forall(p) should === (stream.forall(p))
    }
  }

  test("NonEmptyStream#map is consistent with Stream#map") {
    forAll { (nel: NonEmptyStream[Int], p: Int => String) =>
      val stream = nel.unwrap
      nel.map(p).unwrap should === (stream.map(p))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nel: NonEmptyStream[Int], f: (Int, Int) => Int) =>
      nel.reduceLeft(f) should === (nel.tail.foldLeft(nel.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nel: NonEmptyStream[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      nel.reduceRight(f).value should === (nel.tail.foldRight(nel.head)((a, b) => f(a, Now(b)).value))
    }
  }

  test("reduce consistent with fold") {
    forAll { (nel: NonEmptyStream[Int]) =>
      nel.reduce should === (nel.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nel: NonEmptyStream[Option[Int]]) =>
      nel.reduce(SemigroupK[Option].algebra[Int]) should === (nel.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nel: NonEmptyStream[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nel.tail.foldLeft(Option(f(nel.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nel.reduceLeftToOption(f)(g) should === (expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nel: NonEmptyStream[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val expected = nel.tail.foldRight(Option(f(nel.head))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      nel.reduceRightToOption(f)(g).value should === (expected)
    }
  }
}
