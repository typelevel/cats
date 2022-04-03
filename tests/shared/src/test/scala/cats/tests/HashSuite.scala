package cats.tests

import cats.{Contravariant, Invariant}
import cats.kernel.Hash
import cats.syntax.hash._

class HashSuite extends CatsSuite {

  {
    Invariant[Hash]
    Contravariant[Hash]
  }

  assert(1.hash == 1.hashCode)
  assert("ABC".hash == "ABC".hashCode)

}
