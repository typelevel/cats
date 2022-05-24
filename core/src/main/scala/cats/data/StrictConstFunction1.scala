package cats
package data

/** A `Function1` with constant value and strictly evaluated combinators. Not suitable
 * for composing with side-effecting functions. */
final private[data] case class StrictConstFunction1[A](a: A) extends Function1[Any, A] {
  def apply(arg: Any): A = a

  /** Creates a new `StrictConstFunction1` by applying `g` to this function's constant value.
   * `g` will not be evaluated when the resulting function is subsequently run. Not stack-safe. */
  override def andThen[B](g: A => B): Any => B = StrictConstFunction1(g(a))

  /** This is a no-op; `g` will never be used. */
  override def compose[A0](g: A0 => Any): A0 => A = this
}
