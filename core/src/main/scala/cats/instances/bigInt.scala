package cats
package instances

trait BigIntInstances extends cats.kernel.instances.BigIntInstances {
  implicit val catsStdShowForBigInt: Show[BigInt] =
    Show.fromToString[BigInt]
}
