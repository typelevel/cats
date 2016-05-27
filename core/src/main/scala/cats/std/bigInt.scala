package cats
package std

trait BigIntInstances extends cats.kernel.std.BigIntInstances {
  implicit val catsShowForBigInt: Show[BigInt] =
    Show.fromToString[BigInt]
}
