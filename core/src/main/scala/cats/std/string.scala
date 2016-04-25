package cats
package std

trait StringInstances extends cats.kernel.std.StringInstances {
  implicit val stringShow: Show[String] =
    Show.fromToString[String]
}
