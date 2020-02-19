package cats.tests

import cats.{Contravariant, Invariant}
import cats.instances.all._
import cats.kernel.Hash
import cats.syntax.all._

class HashSuite extends CatsSuite {

  {
    Invariant[Hash]
    Contravariant[Hash]
  }

  assert(1.hash == 1.hashCode)
  assert("ABC".hash == "ABC".hashCode)

}
