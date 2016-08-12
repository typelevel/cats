package cats
package tests

import cats.laws.discipline._

class IdTests extends CatsSuite {
  implicit val iso = CartesianTests.Isomorphisms.invariant[Id]

  checkAll("Id[Int]", BimonadTests[Id].bimonad[Int, Int, Int])
  checkAll("Bimonad[Id]", SerializableTests.serializable(Bimonad[Id]))

  checkAll("Id[Int]", MonadTests[Id].monad[Int, Int, Int])
  checkAll("Monad[Id]", SerializableTests.serializable(Monad[Id]))

  checkAll("Id[Int]", TraverseTests[Id].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Id]", SerializableTests.serializable(Traverse[Id]))
}
