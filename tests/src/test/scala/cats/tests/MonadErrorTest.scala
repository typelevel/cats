package cats
package tests

import data.StateT

import scala.language.postfixOps

class MonadErrorTest extends CatsSuite {

  val successful: Option[Int] = 42.some
  val failed: Option[Int] = None

  test("ensure raises an error if the predicate fails") {
    successful.ensure(())(i => false) should === (None)
  }

  test("ensure returns the successful value if the predicate succeeds") {
    successful.ensure(())(i => true) should === (successful)
  }

  test("ensure returns the failure, when applied to a failure") {
    failed.ensure(())(i => false) should === (failed)
    failed.ensure(())(i => true) should === (failed)
  }

  test("ensureOr raises an error if the predicate fails") {
    successful.ensureOr(_ => ())(_ => false) should === (None)
  }

  test("ensureOr returns the successful value if the predicate succeeds") {
    successful.ensureOr(_ => ())(_ => true) should === (successful)
  }

  test("ensureOr returns the failure, when applied to a failure") {
    failed.ensureOr(_ => ())(_ => false) should === (failed)
    failed.ensureOr(_ => ())(_ => true) should === (failed)
  }

  {
    import StateTTests._

    type Test[A] = StateT[Either[Boolean, ?], Int, A]

    val successful: Test[String] = StateT.modify[Either[Boolean, ?], Int](1 +).map(_ => "foo")
    val failed1: Test[String] = StateT.lift(Left(true))
    val failed2: Test[String] = StateT.lift(Left(false))
    val finalizer: Test[Unit] = StateT.modify[Either[Boolean, ?], Int](10 *)

    test("guarantee returns successful") {
      successful.guarantee(finalizer) should === (successful.flatMap(a => finalizer.map(_ => a)))
    }

    test("guarantee runs finalizer on fail") {
      val expected: Test[String] = for {
        _ <- StateT.modify[Either[Boolean, ?], Int](10 *)
        _ <- failed1
      } yield "foo"

      failed1.guarantee(finalizer) should === (expected)
    }

    test("guarantee returns inner errors on double failure") {
      failed1.guarantee(failed2 >> ().pure[Test]) should === (failed2)
    }
  }
}
