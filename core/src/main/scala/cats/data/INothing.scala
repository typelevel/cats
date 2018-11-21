package cats
package data

sealed trait INothing

object INothing {
  def absurd[A](n: INothing): A = absurd(n)
}
