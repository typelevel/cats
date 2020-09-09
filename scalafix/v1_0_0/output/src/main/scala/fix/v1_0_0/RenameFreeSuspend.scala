package fix
package to1_0_0

object RenameFreeSuspendTests {
  import cats.free.{Free, Trampoline}

  val x = Free.pure[Option, Int](2)
  Free.defer(x)

  Trampoline.defer(Trampoline.done(2))
}
