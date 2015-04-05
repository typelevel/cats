package cats.tests

import cats.laws.discipline._
import cats.laws.discipline.eq._

class FunctionTests extends CatsSuite {
  checkAll("Function0[Int]", ComonadTests[Function0].comonad[Int, Int, Int])
  checkAll("Function0[Int]", MonadTests[Function0].monad[Int, Int, Int])
  checkAll("Function1[Int, Int]", ArrowTests[Function1].arrow[Int, Int, Int, Int, Int, Int])
}
