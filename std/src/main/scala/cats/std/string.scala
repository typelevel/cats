package cats
package std

trait StringInstances extends algebra.std.StringInstances {

  implicit val stringShow: Show[String] = Show.show(identity)

}