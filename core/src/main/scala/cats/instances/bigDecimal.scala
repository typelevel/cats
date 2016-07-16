package cats
package instances

trait BigDecimalInstances {
  implicit val catsStdShowForBigDecimal: Show[BigDecimal] =
    Show.fromToString[BigDecimal]
}
