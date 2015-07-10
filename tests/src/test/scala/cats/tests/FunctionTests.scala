package cats
package tests

import cats.arrow.Arrow
import cats.laws.discipline._
import cats.laws.discipline.eq._

class FunctionTests extends CatsSuite {
  checkAll("Function0[Int]", ComonadTests[Function0].comonad[Int, Int, Int])
  checkAll("Comonad[Function0]", SerializableTests.serializable(Comonad[Function0]))

  checkAll("Function0[Int]", MonadTests[Function0].monad[Int, Int, Int])
  checkAll("Monad[Function0]", SerializableTests.serializable(Monad[Function0]))

  checkAll("Function1[Int, Int]", MonadReaderTests[Function1, Int].monadReader[Int, Int, Int])
  checkAll("MonadReader[Function1, Int]", SerializableTests.serializable(MonadReader[Function1, Int]))

  checkAll("Function1[Int, Int]", ArrowTests[Function1].arrow[Int, Int, Int, Int, Int, Int])
  checkAll("Arrow[Function1]", SerializableTests.serializable(Arrow[Function1]))
}
