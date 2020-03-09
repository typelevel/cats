package cats.tests

import cats.Show
import cats.instances.all._
import ExtraRegressionSuite._

class ExtraRegressionSuite extends CatsSuite {

  /**
   * Placed here to work around scala/bug#6260 on scala 2.10
   */
  test("#1802 duplicated method name") {
    Show[First[String]]
  }
}

object ExtraRegressionSuite {
  final case class First[A](getFirst: A) extends AnyVal
  object First {
    implicit def showInstance[A](implicit ev: Show[A]): Show[First[A]] = new Show[First[A]] {
      override def show(f: First[A]): String = s"First(${ev.show(f.getFirst)})"
    }
  }

}
