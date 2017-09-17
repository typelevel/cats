package cats
package tests

import cats.functor._

class HashTests extends CatsSuite {

  {
    Invariant[Hash]
    Contravariant[Hash]
  }

  assert(1.hash == 1.hashCode)
  assert("ABC".hash == "ABC".hashCode)


}
