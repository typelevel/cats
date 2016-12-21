package cats

import simulacrum.typeclass
import cats.functor.Contravariant

/**
 * A type class to provide textual representation. It is meant to be a
 * better "toString". Whereas toString exists for any Object,
 * regardless of whether or not the creator of the class explicitly
 * made a toString method, a Show instance will only exist if someone
 * explicitly provided one.
 */
@typeclass trait Show[T] {
  def show(f: T): String
}

object Show {
  /** creates an instance of [[Show]] using the provided function */
  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  /** creates an instance of [[Show]] using object toString */
  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  def doShow[A](a: A)(implicit ev: Show[A]): String =
    ev.show(a)

  implicit class ShowInterpolation(val ctx: StringContext) extends AnyVal {
    def show(args: Any*): String = macro cats.macros.ShowInterpolator.show
  }

  implicit val catsContravariantForShow: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
      show[B](fa.show _ compose f)
  }
}
