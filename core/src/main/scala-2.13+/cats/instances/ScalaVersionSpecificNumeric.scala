package cats.instances

abstract private[instances] class ScalaVersionSpecificNumeric[A, B](fa: Numeric[A])(f: A => B)(g: B => A)
    extends Numeric[B] {
  def compare(x: B, y: B): Int = fa.compare(g(x), g(y))
  def plus(x: B, y: B): B = f(fa.plus(g(x), g(y)))
  def minus(x: B, y: B): B = f(fa.minus(g(x), g(y)))
  def times(x: B, y: B): B = f(fa.times(g(x), g(y)))
  def negate(x: B): B = f(fa.negate(g(x)))
  def fromInt(x: Int): B = f(fa.fromInt(x))
  def toInt(x: B): Int = fa.toInt(g(x))
  def toLong(x: B): Long = fa.toLong(g(x))
  def toFloat(x: B): Float = fa.toFloat(g(x))
  def toDouble(x: B): Double = fa.toDouble(g(x))
  def parseString(str: String): Option[B] = fa.parseString(str).map(f)
}
