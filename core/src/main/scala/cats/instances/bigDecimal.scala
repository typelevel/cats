package cats
package instances

trait BigDecimalInstances extends cats.kernel.instances.BigDecimalInstances {
  implicit val catsStdShowForBigDecimal: Show[BigDecimal] =
    Show.fromToString[BigDecimal]
}
