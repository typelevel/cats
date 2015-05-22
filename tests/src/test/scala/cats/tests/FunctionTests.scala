package cats
package tests

import cats.arrow.Arrow
import cats.laws.discipline._
import cats.laws.discipline.eq._

class FunctionTests extends CatsSuite {
  checkAll("Comonad[Function0[?]]", ComonadTests[Function0].comonad[Int, Int, Int])
  checkAll("Serializable[Function0]", SerializableTests.serializable(Comonad[Function0]))

  checkAll("Monad[Function0[?]]", MonadTests[Function0].monad[Int, Int, Int])
  checkAll("Monad[Function0]", SerializableTests.serializable(Monad[Function0]))

  checkAll("Arrow[Function1[Int, Int]]", ArrowTests[Function1].arrow[Int, Int, Int, Int, Int, Int])
  checkAll("Serializable[Function1]", SerializableTests.serializable(Arrow[Function1]))
}
