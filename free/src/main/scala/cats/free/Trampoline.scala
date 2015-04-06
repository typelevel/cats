package cats
package free

object Trampoline {
  def done[A](a: A): Trampoline[A] =
    Free.Pure[Function0,A](a)

  def suspend[A](a: => Trampoline[A]): Trampoline[A] =
    Free.Suspend[Function0, A](() => a)

  def delay[A](a: => A): Trampoline[A] =
    suspend(done(a))
}

