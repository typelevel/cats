package cats
package std

trait BigIntInstances extends algebra.std.BigIntInstances {
  implicit val bigIntShow: Show[BigInt] = new Show[BigInt] {
    def show(f: BigInt): String = f.toString
  }
}
