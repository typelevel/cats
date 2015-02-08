package cats
package syntax

trait ShowSyntax {
  // TODO: use simulacrum instances eventually
  implicit def showSyntax[A: Show](a: A): ShowOps[A] =
    new ShowOps[A](a)
}

class ShowOps[A](a: A)(implicit A: Show[A]) {
  def show: String = A.show(a)
}
