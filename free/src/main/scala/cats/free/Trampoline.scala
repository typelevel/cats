package cats
package free

// To workaround SI-7139 `object Trampoline` needs to be defined inside the package object
// together with the type alias.
abstract private[free] class TrampolineFunctions {
  def done[A](a: A): Trampoline[A] =
    Free.pure[Function0, A](a)

  @deprecated("Use Trampoline.defer.", "1.0.0-MF")
  def suspend[A](a: => Trampoline[A]): Trampoline[A] =
    defer(a)

  def defer[A](a: => Trampoline[A]): Trampoline[A] =
    Free.defer(a)

  def delay[A](a: => A): Trampoline[A] =
    defer(done(a))
}
