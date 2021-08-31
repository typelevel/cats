package cats.tests

import cats.syntax.show._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import scala.collection.immutable.BitSet
import cats.syntax.eq._
import org.scalacheck.Prop._

class BitSetSuite extends CatsSuite {
  implicit val arbitraryBitSet: Arbitrary[BitSet] =
    Arbitrary(arbitrary[List[Short]].map(ns => BitSet(ns.map(_ & 0xffff): _*)))

  test("show BitSet") {
    assert(BitSet(1, 1, 2, 3).show === "BitSet(1, 2, 3)")
    assert(BitSet.empty.show === "BitSet()")

    forAll { (fs: BitSet) =>
      assert(fs.show === (fs.toString))
    }
  }

}
