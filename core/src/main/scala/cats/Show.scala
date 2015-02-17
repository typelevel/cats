package cats

import cats.functor.Contravariant

/**
 * A typeclass to provide textual representation
 */
trait Show[T] {
  def show(f: T): String
}


object Show {
  def apply[T](implicit ev: Show[T]): Show[T] = ev

  /** creates an instance of [[Show]] using the provided function */
  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  /** creates an instance of [[Show]] using object toString */
  def fromToString[A]: Show[A] = show(_.toString)

  implicit val showContravariant: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
      show[B](fa.show _ compose f)
  }

  class ToStringShow[A] extends Show[A] {
    def show(a: A): String = a.toString
  }
}
