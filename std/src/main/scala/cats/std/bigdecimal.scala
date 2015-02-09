package cats
package std

trait BigDecimalInstances /* missing algebra typeclasses */ {
  implicit val bigDecimalShow: Show[BigDecimal] = new Show[BigDecimal] {
    def show(f: BigDecimal): String = f.toString
  }
}
