package cats
package std

trait StringInstances extends cats.kernel.std.StringInstances {
  implicit val catsShowForString: Show[String] =
    Show.fromToString[String]
}
