package cats
package std

trait AnyRefInstances
  extends BigIntInstances
  with    BigDecimalInstances

trait BigIntInstances extends algebra.std.BigIntInstances {

  implicit val bigIntShow: Show[BigInt] = new Show[BigInt] {
    def show(f: BigInt): String = f.toString
  }

}

trait BigDecimalInstances /* missing algebra typeclasses */ {

  implicit val bigDecimalShow: Show[BigDecimal] = new Show[BigDecimal] {
    def show(f: BigDecimal): String = f.toString
  }

}
