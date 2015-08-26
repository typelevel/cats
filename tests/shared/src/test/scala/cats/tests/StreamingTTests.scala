package cats
package tests

import algebra.laws.OrderLaws

import cats.data.StreamingT
import cats.laws.discipline.{CoflatMapTests, EqK, MonadCombineTests, SerializableTests}
import cats.laws.discipline.arbitrary._

class StreamingTTests extends CatsSuite {

  implicit val e0: Eq[StreamingT[Eval, Int]] = EqK[StreamingT[Eval, ?]].synthesize[Int]
  checkAll("StreamingT[Eval, ?]", MonadCombineTests[StreamingT[Eval, ?]].monad[Int, Int, Int])
  checkAll("StreamingT[Eval, ?]", CoflatMapTests[StreamingT[Eval, ?]].coflatMap[Int, Int, Int])
  checkAll("StreamingT[Eval, Int]", OrderLaws[StreamingT[Eval, Int]].order)
  checkAll("Monad[StreamingT[Eval, ?]]", SerializableTests.serializable(Monad[StreamingT[Eval, ?]]))

  implicit val e1: Eq[StreamingT[Option, Int]] = EqK[StreamingT[Option, ?]].synthesize[Int]
  checkAll("StreamingT[Option, ?]", MonadCombineTests[StreamingT[Option, ?]].monad[Int, Int, Int])
  checkAll("StreamingT[Option, ?]", CoflatMapTests[StreamingT[Option, ?]].coflatMap[Int, Int, Int])
  checkAll("StreamingT[Option, Int]", OrderLaws[StreamingT[Option, Int]].order)
  checkAll("Monad[StreamingT[Option, ?]]", SerializableTests.serializable(Monad[StreamingT[Option, ?]]))

  implicit val e2: Eq[StreamingT[List, Int]] = EqK[StreamingT[List, ?]].synthesize[Int]
  checkAll("StreamingT[List, ?]", MonadCombineTests[StreamingT[List, ?]].monad[Int, Int, Int])
  checkAll("StreamingT[List, ?]", CoflatMapTests[StreamingT[List, ?]].coflatMap[Int, Int, Int])
  checkAll("StreamingT[List, Int]", OrderLaws[StreamingT[List, Int]].order)
  checkAll("Monad[StreamingT[List, ?]]", SerializableTests.serializable(Monad[StreamingT[List, ?]]))
}

class SpecificStreamingTTests extends CatsSuite {

  type S[A] = StreamingT[List, A]

  def cons[A](a: A, fs: List[S[A]]): S[A] = StreamingT.cons(a, fs)
  def wait[A](fs: List[S[A]]): S[A] = StreamingT.wait(fs)
  def empty[A]: S[A] = StreamingT.empty[List, A]

  test("counter-example #1"){
    val fa: S[Boolean] =
      cons(true, List(cons(true, List(empty)), empty))

    def f(b: Boolean): S[Boolean] =
      if (b) cons(false, List(cons(true, List(empty))))
      else empty

    def g(b: Boolean): S[Boolean] =
      if (b) empty
      else cons(true, List(cons(false, List(empty)), cons(true, List(empty))))

    val x = fa.flatMap(f).flatMap(g)
    val y = fa.flatMap(a => f(a).flatMap(g))
    assert(x === y)
  }
}
