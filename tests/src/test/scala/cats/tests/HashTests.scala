package cats
package tests

import cats.functor._

class HashTests extends CatsSuite {

  {
    Invariant[Hash]
    Contravariant[Hash]
  }

  //TODO: operator `hash` test

}
