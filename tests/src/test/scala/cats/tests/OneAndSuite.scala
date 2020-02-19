package cats.tests

import cats.{Alternative, Applicative, Foldable, Functor, Monad, SemigroupK, Traverse}
import cats.data.OneAnd
import cats.instances.all._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.all._

class OneAndSuite extends CatsSuite {
  // Lots of collections here.. telling ScalaCheck to calm down a bit
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 20, sizeRange = 5)

  {
    implicit val traverse: Traverse[OneAnd[ListWrapper, *]] = OneAnd.catsDataTraverseForOneAnd(ListWrapper.traverse)
    checkAll("OneAnd[ListWrapper, Int] with Option",
             TraverseTests[OneAnd[ListWrapper, *]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Traverse[OneAnd[ListWrapper, *]]))
  }

  implicit val iso: Isomorphisms[OneAnd[ListWrapper, *]] =
    Isomorphisms.invariant[OneAnd[ListWrapper, *]](OneAnd.catsDataFunctorForOneAnd(ListWrapper.functor))

  // Test instances that have more general constraints
  {
    implicit val monad: Monad[ListWrapper] = ListWrapper.monad
    implicit val alt: Alternative[ListWrapper] = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", MonadTests[OneAnd[ListWrapper, *]].monad[Int, Int, Int])
    checkAll("MonadTests[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Monad[OneAnd[ListWrapper, *]]))
  }

  {
    implicit val alternative: Alternative[ListWrapper] = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", ApplicativeTests[OneAnd[ListWrapper, *]].applicative[Int, Int, Int])
    checkAll("Applicative[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Applicative[OneAnd[ListWrapper, *]]))
  }

  {
    implicit val functor: Functor[ListWrapper] = ListWrapper.functor
    checkAll("OneAnd[ListWrapper, Int]", FunctorTests[OneAnd[ListWrapper, *]].functor[Int, Int, Int])
    checkAll("Functor[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Functor[OneAnd[ListWrapper, *]]))
  }

  {
    implicit val alternative: Alternative[ListWrapper] = ListWrapper.alternative
    checkAll("OneAnd[ListWrapper, Int]", SemigroupKTests[OneAnd[ListWrapper, *]].semigroupK[Int])
    checkAll("SemigroupK[OneAnd[ListWrapper, A]]", SerializableTests.serializable(SemigroupK[OneAnd[ListWrapper, *]]))
  }

  {
    implicit val foldable: Foldable[ListWrapper] = ListWrapper.foldable
    checkAll("OneAnd[ListWrapper, Int]", FoldableTests[OneAnd[ListWrapper, *]].foldable[Int, Int])
    checkAll("Foldable[OneAnd[ListWrapper, A]]", SerializableTests.serializable(Foldable[OneAnd[ListWrapper, *]]))
  }

  test("size is consistent with toList.size") {
    forAll { (oa: OneAnd[Vector, Int]) =>
      oa.size should ===(oa.toList.size.toLong)
    }
  }

}
