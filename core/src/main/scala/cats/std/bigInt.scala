package cats
package std

trait BigIntInstances extends cats.kernel.std.BigIntInstances {
  implicit val bigIntShow: Show[BigInt] =
    Show.fromToString[BigInt]
}
