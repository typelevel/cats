package cats
package tests

import cats.std.all._
import cats.data._

// the things we assert here are not so much important, what is
// important is that this stuff compiles at all.
class UnapplyTests extends CatsSuite {

  test("Unapply works for stuff already the right kind") {
    val x = Traverse[List].traverseU(List(1,2,3))(Option(_))
    assert(x == Some(List(1,2,3)))
  }

  test("Unapply works for F[_,_] with the left fixed") {
    val x = Traverse[List].traverseU(List(1,2,3))(Xor.right(_))
    assert(x == Xor.right(List(1,2,3)))
  }
}
