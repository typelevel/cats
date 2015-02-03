package cats
package std

trait StringInstances extends algebra.std.StringInstances {

  implicit val stringInstance: Show[String] = Show.show(identity)

}