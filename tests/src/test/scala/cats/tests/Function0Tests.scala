package cats.tests

import cats.laws.discipline.{MonadTests, ComonadTests}

class Function0Tests extends CatsSuite {
  checkAll("Function0[Int]", ComonadTests[Function0].comonad[Int, Int, Int])
  checkAll("Function0[Int]", MonadTests[Function0].monad[Int, Int, Int])
}
