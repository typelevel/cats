package cats
package std

trait BigDecimalInstances {
  implicit val bigDecimalShow: Show[BigDecimal] =
    Show.fromToString[BigDecimal]
}
