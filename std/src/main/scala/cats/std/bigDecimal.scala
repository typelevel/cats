package cats
package std

trait BigDecimalInstances {
  implicit val bigDecimalShow: Show[BigDecimal] = new Show[BigDecimal] {
    def show(f: BigDecimal): String = f.toString
  }
}
