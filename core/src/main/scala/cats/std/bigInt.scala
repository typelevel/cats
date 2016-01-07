package cats
package std

trait BigIntInstances {
  implicit val bigIntAlgebra: BigIntAlgebra =
    new BigIntAlgebra

  implicit val bigIntShow: Show[BigInt] =
    Show.fromToString[BigInt]
}

class BigIntAlgebra extends Order[BigInt] {
  def compare(x: BigInt, y: BigInt): Int = x compare y
}
