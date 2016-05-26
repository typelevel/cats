package cats.tests

import cats.{Foldable, Functor, Monad, Traverse}
import cats.data.IdT
import cats.laws.discipline.{CartesianTests, FoldableTests, FunctorTests, MonadTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._

class IdTTests extends CatsSuite {

  implicit val iso = CartesianTests.Isomorphisms.invariant[IdT[List, ?]]

  checkAll("IdT[Functor, Int]", FunctorTests[IdT[List, ?]].functor[Int, Int, Int])
  checkAll("Functor[IdT[List, ?]]", SerializableTests.serializable(Functor[IdT[List, ?]]))

  checkAll("IdT[List, Int]", MonadTests[IdT[List, ?]].monad[Int, Int, Int])
  checkAll("Monad[IdT[List, ?]]", SerializableTests.serializable(Monad[IdT[List, ?]]))

  checkAll("IdT[Option, Int]", FoldableTests[IdT[Option, ?]].foldable[Int, Int])
  checkAll("Foldable[IdT[Option, ?]]", SerializableTests.serializable(Foldable[IdT[Option, ?]]))

  checkAll("IdT[Option, Int]", TraverseTests[IdT[Option, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[IdT[Option, ?]]", SerializableTests.serializable(Traverse[IdT[Option, ?]]))

}
