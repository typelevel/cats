package cats.kernel

package object compat {
  type Stream[+A] = scala.Stream[A]
}
