package cats
package laws
package discipline

import cats.instances.all._
import cats.laws.discipline.arbitrary._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class MonadTestsTests extends AnyFunSuiteLike with FunSuiteDiscipline {
  // We don't use `stackUnsafeMonad` in our laws checking for instances in Cats,
  // so we confirm here that the laws pass for `Eval` (the monad instance for
  // which is actually stack safe, like all other monad instances in Cats.)
  checkAll("Eval[Int]", MonadTests[Eval].stackUnsafeMonad[Int, Int, Int])
}
