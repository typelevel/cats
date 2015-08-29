package cats
package tests

import org.scalactic.anyvals.{PosZDouble, PosInt}

private[tests] object Platform {

  // Override defaults to mimick scalatest 2.2.5 values
  val minSuccessful = PosInt(10)
  val maxDiscardedFactor = PosZDouble(50.0)

  trait UltraSlowCatsSuite extends CatsSuite {
    implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 1, minSuccessful = 1)
  }

  trait  Instances {
    import scala.concurrent.{ExecutionContext, Future}
    import scala.concurrent.duration.FiniteDuration

    def futureEq[A](atMost: FiniteDuration)(implicit A: Eq[A], ec: ExecutionContext): Eq[Future[A]] = ???
    def futureComonad(atMost: FiniteDuration)(implicit ec: ExecutionContext): Comonad[Future] = ???
  }
}
