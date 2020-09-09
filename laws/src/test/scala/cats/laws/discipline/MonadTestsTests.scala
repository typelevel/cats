package cats
package laws
package discipline

import cats.instances.all._
import cats.laws.discipline.arbitrary._
import munit.DisciplineSuite

class MonadTestsTests extends DisciplineSuite {
  // We don't use `stackUnsafeMonad` in our laws checking for instances in Cats,
  // so we confirm here that the laws pass for `Eval` (the monad instance for
  // which is actually stack safe, like all other monad instances in Cats.)
  checkAll("Eval[Int]", MonadTests[Eval].stackUnsafeMonad[Int, Int, Int])
}
