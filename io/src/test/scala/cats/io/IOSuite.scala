package cats
package io

import cats.laws.discipline.arbitrary._

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

trait IOSuite {
  implicit def ioArbitrary[A: Arbitrary]: Arbitrary[IO[A]] =
    Arbitrary(arbitrary[Eval[A]].map(ea => new IO(ea)))

  implicit def ioEq[A: Eq]: Eq[IO[A]] =
    Eq.by(_.unsafePerformIO())
}
