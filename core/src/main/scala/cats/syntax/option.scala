package cats
package syntax

trait OptionSyntax {
  def none[A]: Option[A] = None
}
