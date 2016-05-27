package cats
package std

trait BigIntInstances extends cats.kernel.std.BigIntInstances {
  implicit val catsStdShowForBigInt: Show[BigInt] =
    Show.fromToString[BigInt]
}
