package cats
package tests

import cats.data.EitherT
import cats.laws.discipline._
import cats.kernel.laws.{GroupLaws, OrderLaws}
import scala.util.Try

class EitherTests extends CatsSuite {
  implicit val iso = CartesianTests.Isomorphisms.invariant[Either[Int, ?]]

  checkAll("Either[String, Int]", GroupLaws[Either[String, Int]].monoid)

  checkAll("Either[Int, Int]", CartesianTests[Either[Int, ?]].cartesian[Int, Int, Int])
  checkAll("Cartesian[Either[Int, ?]]", SerializableTests.serializable(Cartesian[Either[Int, ?]]))

  implicit val eq0 = EitherT.catsDataEqForEitherT[Either[Int, ?], Int, Int]

  checkAll("Either[Int, Int]", MonadErrorTests[Either[Int, ?], Int].monadError[Int, Int, Int])
  checkAll("MonadError[Either[Int, ?]]", SerializableTests.serializable(MonadError[Either[Int, ?], Int]))

  checkAll("Either[Int, Int] with Option", TraverseTests[Either[Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Either[Int, ?]", SerializableTests.serializable(Traverse[Either[Int, ?]]))

  checkAll("Either[?, ?]", Traverse2Tests[Either].traverse2[Option, Int, Int, Int, String, String, String])
  checkAll("Traverse2[Either]", SerializableTests.serializable(Traverse2[Either]))

  checkAll("Either[ListWrapper[String], ?]", SemigroupKTests[Either[ListWrapper[String], ?]].semigroupK[Int])
  checkAll("SemigroupK[Either[ListWrapper[String], ?]]", SerializableTests.serializable(SemigroupK[Either[ListWrapper[String], ?]]))

  val partialOrder = catsStdPartialOrderForEither[Int, String]
  val order = implicitly[Order[Either[Int, String]]]
  val monad = implicitly[Monad[Either[Int, ?]]]
  val show = implicitly[Show[Either[Int, String]]]

  {
    implicit val S = ListWrapper.eqv[String]
    implicit val I = ListWrapper.eqv[Int]
    checkAll("Either[ListWrapper[String], ListWrapper[Int]]", OrderLaws[Either[ListWrapper[String], ListWrapper[Int]]].eqv)
    checkAll("Eq[Either[ListWrapper[String], ListWrapper[Int]]]", SerializableTests.serializable(Eq[Either[ListWrapper[String], ListWrapper[Int]]]))
  }

  val orderLaws = OrderLaws[Either[Int, String]]
  checkAll("Either[Int, String]", orderLaws.partialOrder(partialOrder))
  checkAll("Either[Int, String]", orderLaws.order(order))

  test("Left/Right cast syntax") {
    forAll { (e: Either[Int, String]) =>
      e match {
        case l @ Left(_)  =>
          l.rightCast[Double]: Either[Int, Double]
          assert(true)
        case r @ Right(_) =>
          r.leftCast[List[Byte]]: Either[List[Byte], String]
          assert(true)
      }
    }
  }

  test("implicit instances resolve specifically") {
    val eq = catsStdEqForEither[Int, String]
    assert(!eq.isInstanceOf[PartialOrder[_]])
    assert(!eq.isInstanceOf[Order[_]])
    assert(!partialOrder.isInstanceOf[Order[_]])
  }

  test("show isn't empty") {
    forAll { (e: Either[Int, String]) =>
      show.show(e).nonEmpty should === (true)
    }
  }

  test("map2Eval is lazy") {
    val bomb: Eval[Either[String, Int]] = Later(sys.error("boom"))
    val x: Either[String, Int] = Left("l")
    x.map2Eval(bomb)(_ + _).value should === (x)
  }

  test("catchOnly lets non-matching exceptions escape") {
    val _ = intercept[NumberFormatException] {
      Either.catchOnly[IndexOutOfBoundsException]{ "foo".toInt }
    }
  }

  test("catchNonFatal catches non-fatal exceptions") {
    assert(Either.catchNonFatal{ "foo".toInt }.isLeft)
    assert(Either.catchNonFatal{ throw new Throwable("blargh") }.isLeft)
  }

  test("fromTry is left for failed Try") {
    forAll { t: Try[Int] =>
      t.isFailure should === (Either.fromTry(t).isLeft)
    }
  }

  test("fromOption isLeft consistent with Option.isEmpty") {
    forAll { (o: Option[Int], s: String) =>
      Either.fromOption(o, s).isLeft should === (o.isEmpty)
    }
  }

  test("double swap is identity") {
    forAll { (x: Either[Int, String]) =>
      x.swap.swap should === (x)
    }
  }

  test("swap negates isLeft/isRight") {
    forAll { (x: Either[Int, String]) =>
      x.isLeft should !== (x.swap.isLeft)
      x.isRight should !== (x.swap.isRight)
    }
  }

  test("isLeft consistent with isRight") {
    forAll { (x: Either[Int, String]) =>
      x.isLeft should !== (x.isRight)
    }
  }

  test("foreach is noop for left") {
    forAll { (x: Either[Int, String]) =>
      var count = 0
      x.foreach{ _ => count += 1}
      (count == 0) should === (x.isLeft)
    }
  }

  test("getOrElse ignores default for right") {
    forAll { (x: Either[Int, String], s: String, t: String) =>
      if (x.isRight) {
        x.getOrElse(s) should === (x.getOrElse(t))
      }
    }
  }

  test("orElse") {
    forAll { (x: Either[Int, String], y: Either[Int, String]) =>
      val z = x.orElse(y)
      (z === (x)) || (z === (y)) should === (true)
    }
  }

  test("recover recovers handled values") {
    val either = Either.left[String, Int]("either")
    either.recover { case "either" => 5 }.isRight should === (true)
  }

  test("recover ignores unhandled values") {
    val either = Either.left[String, Int]("either")
    either.recover { case "noteither" => 5 } should === (either)
  }

  test("recover ignores the right side") {
    val either = Either.right[String, Int](10)
    either.recover { case "either" => 5 } should === (either)
  }

  test("recoverWith recovers handled values") {
    val either = Either.left[String, Int]("either")
    either.recoverWith { case "either" => Either.right[String, Int](5) }.isRight should === (true)
  }

  test("recoverWith ignores unhandled values") {
    val either = Either.left[String, Int]("either")
    either.recoverWith { case "noteither" => Either.right[String, Int](5) } should === (either)
  }

  test("recoverWith ignores the right side") {
    val either = Either.right[String, Int](10)
    either.recoverWith { case "either" => Either.right[String, Int](5) } should === (either)
  }

  test("valueOr consistent with swap then map then merge") {
    forAll { (x: Either[Int, String], f: Int => String) =>
      x.valueOr(f) should === (x.swap.map(f).merge)
    }
  }

  test("isLeft implies forall") {
    forAll { (x: Either[Int, String], p: String => Boolean) =>
      if (x.isLeft) {
        x.forall(p) should === (true)
      }
    }
  }

  test("isLeft implies exists is false") {
    forAll { (x: Either[Int, String], p: String => Boolean) =>
      if (x.isLeft) {
        x.exists(p) should === (false)
      }
    }
  }

  test("ensure on left is identity") {
    forAll { (x: Either[Int, String], i: Int, p: String => Boolean) =>
      if (x.isLeft) {
        x.ensure(i)(p) should === (x)
      }
    }
  }

  test("toIor then toEither is identity") {
    forAll { (x: Either[Int, String]) =>
      x.toIor.toEither should === (x)
    }
  }

  test("toTry then fromTry is identity") {
    implicit def eqTh: Eq[Throwable] = Eq.allEqual

    forAll { (x: Throwable Either String) =>
      Either.fromTry(x.toTry) should === (x)
    }
  }

  test("isLeft consistency") {
    forAll { (x: Either[Int, String]) =>
      x.isLeft should === (x.toOption.isEmpty)
      x.isLeft should === (x.toList.isEmpty)
      x.isLeft should === (x.toValidated.isInvalid)
      x.isLeft should === (x.toValidatedNel.isInvalid)
      Option(x.isLeft) should === (x.toEitherT[Option].isLeft)
    }
  }

  test("withValidated") {
    forAll { (x: Either[Int, String], f: Int => Double) =>
      x.withValidated(_.map2(f, identity)) should === (x.leftMap(f))
    }
  }

  test("combine is right iff both operands are right") {
    forAll { (x: Either[Int, String], y: Either[Int, String]) =>
      x.combine(y).isRight should === (x.isRight && y.isRight)
    }
  }

  test("to consistent with toList") {
    forAll { (x: Either[Int, String]) =>
      x.to[List] should === (x.toList)
    }
  }

  test("to consistent with toOption") {
    forAll { (x: Either[Int, String]) =>
      x.to[Option] should === (x.toOption)
    }
  }
}
