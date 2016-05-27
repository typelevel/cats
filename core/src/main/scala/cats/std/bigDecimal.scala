package cats
package std

trait BigDecimalInstances {
  implicit val catsShowForBigDecimal: Show[BigDecimal] =
    Show.fromToString[BigDecimal]
}
