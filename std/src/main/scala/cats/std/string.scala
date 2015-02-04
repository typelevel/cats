package cats
package std

trait StringInstances extends algebra.std.StringInstances {

  implicit val stringShow: Show[String] = new Show[String] {
    def show(s: String): String = s
  }

}