package cats
package tests

class IsSuite extends CatsSuite {
  import evidence._

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
