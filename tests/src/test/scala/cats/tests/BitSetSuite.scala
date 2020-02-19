package cats.tests

import cats.instances.all._
import cats.syntax.all._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import scala.collection.immutable.BitSet

class BitSetSuite extends CatsSuite {
  implicit val arbitraryBitSet: Arbitrary[BitSet] =
    Arbitrary(arbitrary[List[Short]].map(ns => BitSet(ns.map(_ & 0xffff): _*)))

  test("show BitSet") {
    BitSet(1, 1, 2, 3).show should ===("BitSet(1, 2, 3)")
    BitSet.empty.show should ===("BitSet()")

    forAll { (fs: BitSet) =>
      fs.show should ===(fs.toString)
    }
  }

}
