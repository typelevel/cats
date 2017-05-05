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
@typeclass trait Show[T] extends Show.ContravariantShow[T] {
  // duplicated so simulacrum knows it requires an instance of this trait
  def show(t: T): String
}

object Show {
  trait ContravariantShow[-T] {
    def show(t: T): String
  }

  /** creates an instance of [[Show]] using the provided function */
  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  /** creates an instance of [[Show]] using object toString */
  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  final case class Shown(override val toString: String) extends AnyVal
  object Shown {
    implicit def mat[A](x: A)(implicit z: ContravariantShow[A]): Shown = Shown(z show x)
  }

  final case class ShowInterpolator(_sc: StringContext) extends AnyVal {
    def show(args: Shown*): String = _sc s (args: _*)
  }

  implicit val catsContravariantForShow: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] =
      show[B](fa.show _ compose f)
  }
}
