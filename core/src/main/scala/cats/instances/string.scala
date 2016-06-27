package cats
package instances

trait StringInstances extends cats.kernel.instances.StringInstances {
  implicit val catsStdShowForString: Show[String] =
    Show.fromToString[String]
}
