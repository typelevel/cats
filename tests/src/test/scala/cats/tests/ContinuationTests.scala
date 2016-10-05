package cats
package tests

import cats.data.Continuation
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._

class ContinuationTests extends CatsSuite {
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
  test("Continuation.always works") {
    var cnt = 0
    val c = Continuation.always[Int, Int] { cnt += 1; 0 }
    assert(cnt == 0)
    c(identity)
    assert(cnt == 1)
    c(identity)
    assert(cnt == 2)
  }
  test("Continuation.later works") {
    var cnt = 0
    val c = Continuation.later[Int, Int] { cnt += 1; 0 }
    assert(cnt == 0)
    c(identity)
    assert(cnt == 1)
    c(identity)
    assert(cnt == 1)
  }
  test("Continuation.fromEval works") {
    var cnt = 0
    val c = Continuation.fromEval[Int, Int](Eval.later { cnt += 1; 0 })
    assert(cnt == 0)
    c(identity)
    assert(cnt == 1)
    c(identity)
    assert(cnt == 1)
  }
  test("Continuation.fixed works") {
    val c = Continuation.fixed(100).widen[String] // compiler sees Nothing and warns on calling it, since any call is dead code
    assert(c(_ => sys.error("never called")) == 100)
  }
}
