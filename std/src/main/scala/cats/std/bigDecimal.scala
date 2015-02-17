package cats
package std

trait BigDecimalInstances {
  implicit val bigDecimalShow: Show[BigDecimal] =
    new Show.ToStringShow[BigDecimal]
}
