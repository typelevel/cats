package cats.kernel

/** ADT encoding the possible results of a comparison */
sealed abstract class Comparison extends Product with Serializable {
  /** The signum of this comparison */
  def toInt: Int = this match {
    case Comparison.GreaterThan => 1
    case Comparison.EqualTo     => 0
    case Comparison.LessThan    => -1
  }
}

object Comparison {
  final case object GreaterThan extends Comparison
  final case object EqualTo     extends Comparison
  final case object LessThan    extends Comparison

  implicit val catsKernelEqForComparison: Eq[Comparison] = Eq.fromUniversalEquals
}
