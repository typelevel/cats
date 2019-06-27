
import cats.kernel.laws.discipline.{EqTests, SemigroupTests}

import cats.data.OneAnd
import cats.`scala-2.13+`.data.NonEmptyLazyList // TODO how to import / does it even belong there?
import cats.laws.discipline.{
  ApplicativeTests,
  ComonadTests,
  FoldableTests,
  FunctorTests,
  MonadTests,
  NonEmptyTraverseTests,
  ReducibleTests,
  SemigroupKTests,
  SemigroupalTests,
  SerializableTests,
  TraverseTests
}

// TODO not sure how to convert some of these `OneAnd` specific-tests
class OneAndSuite extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 5)

  checkAll("OneAnd[Stream, Int]", EqTests[OneAnd[LazyList, Int]].eqv)

  checkAll("OneAnd[Stream, Int] with Option",
           NonEmptyTraverseTests[OneAnd[LazyList, ?]].nonEmptyTraverse[Option, Int, Int, Int, Int, Option, Option])
  checkAll("NonEmptyTraverse[OneAnd[Stream, A]]", SerializableTests.serializable(NonEmptyTraverse[OneAnd[LazyList, ?]]))

  {
    implicit val traverse = OneAnd.catsDataTraverseForOneAnd(ListWrapper.traverse)
    checkAll("OneAnd[ListWrapper, Int] with Option",
             TraverseTests[OneAnd[ListWrapper, ?]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Traverse[OneAnd[ListWrapper, ?]]))
  }

  checkAll("OneAnd[LazyList, Int]", ReducibleTests[OneAnd[LazyList, ?]].reducible[Option, Int, Int])
  checkAll("Reducible[OneAnd[LazyList, ?]]", SerializableTests.serializable(Reducible[OneAnd[LazyList, ?]]))

  implicit val iso = SemigroupalTests.Isomorphisms
    .invariant[OneAnd[ListWrapper, ?]](OneAnd.catsDataFunctorForOneAnd(ListWrapper.functor))

  // Test instances that have more general constraints
  {
    implicit val monad = ListWrapper.monad
    implicit val alt = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", MonadTests[OneAnd[ListWrapper, ?]].monad[Int, Int, Int])
    checkAll("MonadTests[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Monad[OneAnd[ListWrapper, ?]]))
  }

  {
    implicit val alternative = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", ApplicativeTests[OneAnd[ListWrapper, ?]].applicative[Int, Int, Int])
    checkAll("Applicative[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Applicative[OneAnd[ListWrapper, ?]]))
  }

  {
    implicit val functor = ListWrapper.functor
    checkAll("OneAnd[ListWrapper, Int]", FunctorTests[OneAnd[ListWrapper, ?]].functor[Int, Int, Int])
    checkAll("Functor[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Functor[OneAnd[ListWrapper, ?]]))
  }

  {
    implicit val alternative = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", SemigroupKTests[OneAnd[ListWrapper, ?]].semigroupK[Int])
    checkAll("OneAnd[LazyList, Int]", SemigroupTests[OneAnd[LazyList, Int]].semigroup)
    checkAll("SemigroupK[OneAnd[ListWrapper, A]]", SerializableTests.serializable(SemigroupK[OneAnd[ListWrapper, ?]]))
    checkAll("Semigroup[NonEmptyLazyList[Int]]", SerializableTests.serializable(Semigroup[OneAnd[LazyList, Int]]))
  }

  {
    implicit val foldable = ListWrapper.foldable
    checkAll("OneAnd[ListWrapper, Int]", FoldableTests[OneAnd[ListWrapper, ?]].foldable[Int, Int])
    checkAll("Foldable[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Foldable[OneAnd[ListWrapper, ?]]))
  }

  {
    // Test functor and subclasses don't have implicit conflicts
    implicitly[Functor[NonEmptyLazyList]]
    implicitly[Monad[NonEmptyLazyList]]
    implicitly[Comonad[NonEmptyLazyList]]
  }

  implicit val iso2 = SemigroupalTests.Isomorphisms.invariant[OneAnd[LazyList, ?]]

  //OneAnd's tailRecM fails on LazyList due to the fact that. todo: replace NonEmptyStream with NonEmptyLazyList using newtype https://github.com/typelevel/cats/issues/2903
  checkAll("NonEmptyLazyList[Int]", MonadTests[NonEmptyLazyList].stackUnsafeMonad[Int, Int, Int])
  checkAll("Monad[NonEmptyLazyList[A]]", SerializableTests.serializable(Monad[NonEmptyLazyStream]))

  checkAll("NonEmptyLazyList[Int]", ComonadTests[NonEmptyLazyList].comonad[Int, Int, Int])
  checkAll("Comonad[NonEmptyLazyList[A]]", SerializableTests.serializable(Comonad[NonEmptyLazyList]))

  test("size is consistent with toList.size") {
    forAll { (oa: OneAnd[Vector, Int]) =>
      oa.size should ===(oa.toList.size.toLong)
    }
  }

  test("Show is not empty and is formatted as expected") {
    forAll { (nel: NonEmptyLazyList[Int]) =>
      nel.show.nonEmpty should ===(true)
      nel.show.startsWith("NonEmptyLazyList(") should ===(true)
      nel.show should ===(implicitly[Show[NonEmptyLazyList[Int]]].show(nel))
      nel.show.contains(nel.head.show) should ===(true)
    }
  }

  test("Show is formatted correctly") {
    val oneAnd = NonEmptyLazyList("Test")
    oneAnd.show should ===(s"OneAnd(Test, ${compat.lazyList.lazyListString}())")
  }

  test("Creating OneAnd + unwrap is identity") {
    forAll { (i: Int, tail: LazyList[Int]) =>
      val stream = i #:: tail
      val oneAnd = NonEmptyLazyList(i, tail: _*)
      stream should ===(oneAnd.unwrap)
    }
  }

  test("NonEmptyLazyList#find is consistent with LazyList#find") {
    forAll { (nell: NonEmptyLazyList[Int], p: Int => Boolean) =>
      val lazyList = nell.unwrap
      nell.find(p) should ===(lazyList.find(p))
    }
  }

  test("NonEmptyLazyList#exists is consistent with LazyList#exists") {
    forAll { (nell: NonEmptyLazyList[Int], p: Int => Boolean) =>
      val lazyList = nell.unwrap
      nell.exists(p) should ===(lazyList.exists(p))
    }
  }

  test("NonEmptyLazyList#forall is consistent with LazyList#forall") {
    forAll { (nell: NonEmptyLazyList[Int], p: Int => Boolean) =>
      val lazyList = nell.unwrap
      nell.forall(p) should ===(lazyList.forall(p))
    }
  }

  test("NonEmptyLazy#map is consistent with LazyList#map") {
    forAll { (nell: NonEmptyLazyList[Int], p: Int => String) =>
      val lazyList = nel.unwrap
      nell.map(p).unwrap should ===(lazyList.map(p))
    }
  }

  test("NonEmptyLazyList#nonEmptyPartition remains sorted") {
    forAll { (nell: NonEmptyLazyList[Int], f: Int => Either[String, String]) =>
      val nellf = nes.map(f)
      val sortedLazyList = (nellf.head #:: nellf.tail).sorted
      val sortedNell = OneAnd(sortedLazyList.head, sortedLazyList.tail)
      val ior = Reducible[NonEmptyLazyList].nonEmptyPartition(sortedNell)(identity)

      ior.left.map(xs => xs.sorted should ===(xs))
      ior.right.map(xs => xs.sorted should ===(xs))
    }
  }

  test("reduceLeft consistent with foldLeft") {
    forAll { (nell: NonEmptyLazyList[Int], f: (Int, Int) => Int) =>
      nell.reduceLeft(f) should ===(nell.tail.foldLeft(nell.head)(f))
    }
  }

  test("reduceRight consistent with foldRight") {
    forAll { (nell: NonEmptyLazyList[Int], f: (Int, Eval[Int]) => Eval[Int]) =>
      val got = nell.reduceRight(f).value
      val last :: rev = nell.unwrap.toList.reverse
      val expected = rev.reverse.foldRight(last)((a, b) => f(a, Now(b)).value)
      got should ===(expected)
    }
  }

  test("reduce consistent with fold") {
    forAll { (nell: NonEmptyLazyList[Int]) =>
      nell.reduce should ===(nel.fold)
    }
  }

  test("reduce consistent with reduceK") {
    forAll { (nell: NonEmptyLazyList[Option[Int]]) =>
      nell.reduce(SemigroupK[Option].algebra[Int]) should ===(nel.reduceK)
    }
  }

  test("reduceLeftToOption consistent with foldLeft + Option") {
    forAll { (nell: NonEmptyLazyList[Int], f: Int => String, g: (String, Int) => String) =>
      val expected = nell.tail.foldLeft(Option(f(nel.head))) { (opt, i) =>
        opt.map(s => g(s, i))
      }
      nell.reduceLeftToOption(f)(g) should ===(expected)
    }
  }

  test("reduceRightToOption consistent with foldRight + Option") {
    forAll { (nell: NonEmptyLazyList[Int], f: Int => String, g: (Int, Eval[String]) => Eval[String]) =>
      val got = nell.reduceRightToOption(f)(g).value
      val last :: rev = nell.unwrap.toList.reverse
      val expected = rev.reverse.foldRight(Option(f(last))) { (i, opt) =>
        opt.map(s => g(i, Now(s)).value)
      }
      got should ===(expected)
    }
  }

  test("filter includes elements based on a predicate") {
    forAll { (nell: NonEmptyLazyList[Int], pred: Int => Boolean) =>
      nell.filter(pred) should ===(nell.unwrap.filter(pred))
    }
  }

}

class ReducibleNonEmptyLazyListSuite extends ReducibleSuite[NonEmptyLazyList]("NonEmptyLazyList") {
  def iterator[T](nell: NonEmptyLazyList[T]): Iterator[T] =
    (nell.head #:: nell.tail).iterator

  def range(start: Long, endInclusive: Long): NonEmptyLazyList[Long] = {
    // if we inline this we get a bewildering implicit numeric widening
    // error message in Scala 2.10
    val tailStart: Long = start + 1L
    NonEmptyLazyList(start, toLazyList(tailStart.to(endInclusive)))
  }
}
