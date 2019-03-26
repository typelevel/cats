package cats
package tests

import cats.arrow._
import cats.kernel.laws.discipline.SerializableTests

class IsSuite extends CatsSuite {
  import evidence._

  checkAll("Category[Is]", SerializableTests.serializable(Category[Is]))

  test("syntax") {
    trait Bar

    val lifted: Bar Is Bar = Is.refl[Bar]
    val andThen: Leibniz[Bar, Bar] = lifted.andThen(lifted)
    val compose: Leibniz[Bar, Bar] = lifted.compose(lifted)
    val flip: Leibniz[Bar, Bar] = lifted.flip
    val lift: Leibniz[List[Bar], List[Bar]] = lifted.lift[List]
    val coerce: Bar = lifted.coerce(new Bar {})
    val predefEq: =:=[Bar, Bar] = lifted.predefEq
  }

}
