/*
rule = "scala:fix.v1_0_0.RenameFreeSuspend"
 */
package fix
package to1_0_0

object RenameFreeSuspendTests {
  import cats.free.{Free, Trampoline}

  val x = Free.pure[Option, Int](2)
  Free.suspend(x)

  Trampoline.suspend(Trampoline.done(2))
}
