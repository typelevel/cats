package cats
package std

trait StringInstances extends cats.kernel.std.StringInstances {
  implicit val catsStdShowForString: Show[String] =
    Show.fromToString[String]
}
