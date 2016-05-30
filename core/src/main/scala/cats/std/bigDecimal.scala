package cats
package std

trait BigDecimalInstances {
  implicit val catsStdShowForBigDecimal: Show[BigDecimal] =
    Show.fromToString[BigDecimal]
}
