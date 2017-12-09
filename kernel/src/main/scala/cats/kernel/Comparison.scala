package cats.kernel

/** ADT encoding the possible results of a comparison */
sealed abstract class Comparison(val toInt: Int, val toDouble: Double) extends Product with Serializable

object Comparison {
  final case object GreaterThan extends Comparison(1, 1.0)
  final case object EqualTo     extends Comparison(0, 0.0)
  final case object LessThan    extends Comparison(-1, -1.0)

  // Used for fromDouble
  private val SomeGt = Some(Comparison.GreaterThan)
  private val SomeEq = Some(Comparison.EqualTo)
  private val SomeLt = Some(Comparison.LessThan)

  def fromInt(int: Int): Comparison = {
    if (int > 0)       Comparison.GreaterThan
    else if (int == 0) Comparison.EqualTo
    else               Comparison.LessThan // scalastyle:ignore ensure.single.space.after.token
  }

  def fromDouble(double: Double): Option[Comparison] = {
    if (double.isNaN) None
    else if (double > 0.0) SomeGt
    else if (double == 0.0) SomeEq
    else SomeLt
  }

  implicit val catsKernelInstancesForComparison: Eq[Comparison] with Monoid[Comparison] =
    new Eq[Comparison] with Monoid[Comparison] {
      def eqv(x: Comparison, y: Comparison): Boolean = x == y
      def empty: Comparison = EqualTo

      def combine(x: Comparison, y: Comparison): Comparison = x match {
        case EqualTo => y
        case comp    => comp
      }
    }
}
