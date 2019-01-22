package cats
package tests

import cats.data.{IndexedStoreT, NonEmptyList, StoreId}
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import cats.instances.tuple._
import org.scalacheck.{Arbitrary, Cogen}

class IndexedStoreTSuite extends CatsSuite {

  import IndexedStoreTSuite._

  test("basic store usage") {
    simpleStore.pos should ===(1 -> 5)
  }

  {
    // We only need a Functor to derive a Bifunctor for IndexedStoreT
    implicit val F: Functor[NonEmptyListWrapper] = NonEmptyListWrapper.comonad
    Bifunctor[IndexedStoreT[NonEmptyListWrapper, ?, Int, ?]]
  }

  {
    implicit val F: Comonad[NonEmptyListWrapper] = NonEmptyListWrapper.comonad
    implicit val FS: Functor[IndexedStoreT[NonEmptyListWrapper, String, Int, ?]] =
      IndexedStoreT.catsDataFunctorRightForIndexedStoreT

    checkAll("IndexedStoreT[NonEmptyListWrapper, String, Int, ?]",
             FunctorTests[IndexedStoreT[NonEmptyListWrapper, String, Int, ?]].functor[Int, Int, Int])
    checkAll(
      "Functor[IndexedStoreT[NonEmptyListWrapper, String, Int, ?]]",
      SerializableTests.serializable(Functor[IndexedStoreT[NonEmptyListWrapper, String, Int, ?]])
    )

    Functor[IndexedStoreT[NonEmptyListWrapper, String, Int, ?]]
  }

  {
    implicit val F: Comonad[NonEmptyListWrapper] = NonEmptyListWrapper.comonad
    implicit val FS: Contravariant[IndexedStoreT[NonEmptyListWrapper, Int, ?, Int]] =
      IndexedStoreT.catsDataContravariantForIndexedStoreT

    checkAll("IndexedStoreT[NonEmptyListWrapper, Int, ?, Int]",
             ContravariantTests[IndexedStoreT[NonEmptyListWrapper, Int, ?, Int]].contravariant[Int, Int, Int])
    checkAll(
      "Contravariant[IndexedStoreT[NonEmptyListWrapper, Int, ?, Int]]",
      SerializableTests.serializable(Contravariant[IndexedStoreT[NonEmptyListWrapper, Int, ?, Int]])
    )

    Contravariant[IndexedStoreT[NonEmptyListWrapper, Int, ?, Int]]
  }

  {
    implicit val F: Comonad[NonEmptyListWrapper] = NonEmptyListWrapper.comonad
    implicit val FS: Bifunctor[IndexedStoreT[NonEmptyListWrapper, ?, Int, ?]] =
      IndexedStoreT.catsDataBifunctorForIndexedStoreT

    checkAll(
      "IndexedStoreT[NonEmptyListWrapper, ?, Int, ?]",
      BifunctorTests[IndexedStoreT[NonEmptyListWrapper, ?, Int, ?]].bifunctor[String, String, String, Int, Int, Int]
    )
    checkAll("Bifunctor[IndexedStoreT[NonEmptyListWrapper, ?, Int, ?]]",
             SerializableTests.serializable(Bifunctor[IndexedStoreT[NonEmptyListWrapper, ?, Int, ?]]))

    Bifunctor[IndexedStoreT[NonEmptyListWrapper, ?, Int, ?]]
  }

  checkAll("StoreId[Int, ?]", ComonadTests[StoreId[Int, ?]].comonad[Int, Int, Int])
  checkAll("Comonad[StoreId[Long, ?]]", SerializableTests.serializable(Comonad[StoreId[Long, ?]]))
}

object IndexedStoreTSuite extends IndexedStoreTSuiteInstances {
  implicit def storeEq[S: Eq: Arbitrary, A: Eq]: Eq[StoreId[S, A]] =
    indexedStoreTEq[Eval, S, S, A]

  val simpleStore: StoreId[(Int, Int), Int] = StoreId(_._1, (1, 5))
}

sealed trait IndexedStoreTSuiteInstances {

  implicit def indexedStoreTEq[F[_], I, A, B](implicit I: Arbitrary[I],
                                              EqI: Eq[I],
                                              FAB: Eq[F[A => B]]): Eq[IndexedStoreT[F, I, A, B]] =
    Eq.by[IndexedStoreT[F, I, A, B], (F[A => B], I)](_.run)
}

/** This data type exists purely for testing.
 *
 */
final case class NonEmptyListWrapper[A](nonEmptyList: NonEmptyList[A]) extends AnyVal

object NonEmptyListWrapper {

  import org.scalacheck.Arbitrary.arbitrary

  val comonad: Comonad[NonEmptyListWrapper] = new Comonad[NonEmptyListWrapper] {
    val CM = Comonad[NonEmptyList]

    def extract[A](wrapper: NonEmptyListWrapper[A]): A = wrapper.nonEmptyList.head

    def coflatMap[A, B](fa: NonEmptyListWrapper[A])(
      f: NonEmptyListWrapper[A] => B
    ): NonEmptyListWrapper[B] =
      NonEmptyListWrapper(fa.nonEmptyList.coflatMap(nel => f(NonEmptyListWrapper(nel))))

    def map[A, B](fa: NonEmptyListWrapper[A])(f: A => B): NonEmptyListWrapper[B] =
      NonEmptyListWrapper(CM.map(fa.nonEmptyList)(f))

  }

  implicit def nonEmptyListWrapperArbitrary[B: Arbitrary]: Arbitrary[NonEmptyListWrapper[B]] =
    Arbitrary(arbitrary[NonEmptyList[B]].map(NonEmptyListWrapper.apply))

  implicit def nonEmptyListWrapperCogen[A: Cogen]: Cogen[NonEmptyListWrapper[A]] =
    Cogen[NonEmptyList[A]].contramap(_.nonEmptyList)

  implicit def nonEmptyListWrapperEq[A: Eq]: Eq[NonEmptyListWrapper[A]] =
    Eq.by(_.nonEmptyList)
}
