package cats
package std

trait BigIntInstances extends algebra.std.BigIntInstances {
  implicit val bigIntShow: Show[BigInt] =
    Show.fromToString[BigInt]
}
