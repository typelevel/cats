package cats
package tests

import scala.util.control.TailCalls.{done, tailcall, TailRec}
import org.scalacheck.{Arbitrary, Cogen, Gen}

import Arbitrary.arbitrary

import cats.laws.discipline.{DeferTests, MonadTests, SerializableTests}

class TailRecSuite extends CatsSuite {

  implicit def tailRecArb[A: Arbitrary: Cogen]: Arbitrary[TailRec[A]] =
    Arbitrary(
      Gen.frequency(
        (3, arbitrary[A].map(done(_))),
        (1, Gen.lzy(arbitrary[(A, A => TailRec[A])].map { case (a, fn)          => tailcall(fn(a)) })),
        (1, Gen.lzy(arbitrary[(TailRec[A], A => TailRec[A])].map { case (a, fn) => a.flatMap(fn) }))
      )
    )

  implicit def eqTailRec[A: Eq]: Eq[TailRec[A]] =
    Eq.by[TailRec[A], A](_.result)

  checkAll("TailRec[Int]", MonadTests[TailRec].monad[Int, Int, Int])
  checkAll("Monad[TailRec]", SerializableTests.serializable(Monad[TailRec]))

  checkAll("TailRec[Int]", DeferTests[TailRec].defer[Int])
  checkAll("Defer[TailRec]", SerializableTests.serializable(Defer[TailRec]))
}
