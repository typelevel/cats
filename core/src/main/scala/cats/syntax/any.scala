package cats
package syntax

trait AnySyntax {
  implicit def anySyntax[A](a: A): AnyOps[A] =
    new AnyOps[A](a)
}

class AnyOps[A](val a: A) extends AnyVal {
  def some: Option[A] = Some(a)
}
