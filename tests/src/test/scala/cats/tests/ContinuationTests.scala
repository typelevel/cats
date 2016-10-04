package cats
package tests

import cats.data.Continuation
import cats.laws.discipline._
import org.scalacheck.{ Arbitrary, Gen }

class ContinuationTests extends CatsSuite {

  implicit def arbContFn[O, I](implicit I: Arbitrary[I], O: Arbitrary[O]): Arbitrary[(I => O) => O] = {
    def call(i: I): (I => O) => O = { fn => fn(i) }
    def const(o: O): (I => O) => O = { fn => o }

    Arbitrary(Gen.oneOf(I.arbitrary.map(call(_)), O.arbitrary.map { o => const(o) }))
  }
  implicit def arbCont[O, I](implicit I: Arbitrary[I], fn: Arbitrary[(I => O) => O]): Arbitrary[Continuation[O, I]] =
    Arbitrary(Gen.oneOf(I.arbitrary.map(Continuation.pure[O](_)),
      fn.arbitrary.map(Continuation.from(_))))

  def eq0[I, O: Eq](fn: I => O) = new Eq[Continuation[O, I]] {
    def eqv(a: Continuation[O, I], b: Continuation[O, I]) =
      Eq[O].eqv(a(fn), b(fn))
  }
  implicit val eqInt = eq0[Int, Int](x => x*x*x)
  implicit val eqInt3 = eq0[(Int, Int, Int), Int] { case (x, y, z) => x*y*z }
  implicit val iso = CartesianTests.Isomorphisms.invariant[Continuation[Int, ?]]

  checkAll("Continuation[Int, ?]", MonadTests[Continuation[Int, ?]].monad[Int, Int, Int])
  checkAll("Monad[Continuation[Int, ?]]", SerializableTests.serializable(Monad[Continuation[Int, ?]]))

  test("continuations are called") {
    val c = Continuation.from2[Int, Int, Int]((0 to 100).reduce(_))
    assert(c { case (a, b) => a + b } == (0 to 100).reduce(_ + _))
  }
  test("flatMaps are sane") {
    val exists = Continuation.from[Int, Boolean](List(1, 2, 3, 4).exists(_))
    val forall = Continuation.from[Int, Boolean](List(1, 2, 3, 4).forall(_))
    val c = for {
      e <- exists
      f <- forall
    } yield (e, f)

    // Does there exist a number that is <= all the numbers? yes.
    assert(c { case (x, y) => (x <= y) } == true)
    // Does there exist a number that is >= all the numbers? yes.
    assert(c { case (x, y) => (x >= y) } == true)
    // Does there exist a number that is > all the numbers? no.
    assert(c { case (x, y) => (x == y) } == false)
  }
}
