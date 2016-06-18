package cats
package instances

trait StringInstances extends cats.kernel.std.StringInstances {
  implicit val catsStdShowForString: Show[String] =
    Show.fromToString[String]
}
