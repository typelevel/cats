package cats
package std

trait StringInstances {
  implicit val stringInstance: Show[String] =
    new Show[String] {
      def show(s: String) = s
    }
}
