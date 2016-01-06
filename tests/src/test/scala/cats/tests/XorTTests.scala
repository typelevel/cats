package cats
package tests

import cats.functor.Bifunctor
import cats.data.{Xor, XorT}
import cats.laws.OrderLaws
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq.tuple3Eq

class XorTTests extends CatsSuite {
  implicit val eq0 = XorT.xorTEq[List, String, String Xor Int]
  implicit val eq1 = XorT.xorTEq[XorT[List, String, ?], String, Int](eq0)
  implicit val iso = MonoidalTests.Isomorphisms.invariant[XorT[List, String, ?]]
  checkAll("XorT[List, String, Int]", MonadErrorTests[XorT[List, String, ?], String].monadError[Int, Int, Int])
  checkAll("MonadError[XorT[List, ?, ?]]", SerializableTests.serializable(MonadError[XorT[List, String, ?], String]))
  checkAll("XorT[List, String, Int]", MonoidKTests[XorT[List, String, ?]].monoidK[Int])
  checkAll("MonoidK[XorT[List, String, ?]]", SerializableTests.serializable(MonoidK[XorT[List, String, ?]]))
  checkAll("XorT[List, ?, ?]", BifunctorTests[XorT[List, ?, ?]].bifunctor[Int, Int, Int, String, String, String])
  checkAll("Bifunctor[XorT[List, ?, ?]]", SerializableTests.serializable(Bifunctor[XorT[List, ?, ?]]))
  checkAll("XorT[List, Int, ?]", TraverseTests[XorT[List, Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[XorT[List, Int, ?]]", SerializableTests.serializable(Traverse[XorT[List, Int, ?]]))
  checkAll("XorT[List, String, Int]", OrderLaws[XorT[List, String, Int]].order)
  checkAll("Order[XorT[List, String, Int]]", SerializableTests.serializable(Order[XorT[List, String, Int]]))

  {
    implicit val F = ListWrapper.foldable
    checkAll("XorT[ListWrapper, Int, ?]", FoldableTests[XorT[ListWrapper, Int, ?]].foldable[Int, Int])
    checkAll("Foldable[XorT[ListWrapper, Int, ?]]", SerializableTests.serializable(Foldable[XorT[ListWrapper, Int, ?]]))
  }

  {
    implicit val F = ListWrapper.functor
    checkAll("XorT[ListWrapper, Int, ?]", FunctorTests[XorT[ListWrapper, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[XorT[ListWrapper, Int, ?]]", SerializableTests.serializable(Functor[XorT[ListWrapper, Int, ?]]))
  }

  {
    implicit val F = ListWrapper.partialOrder[String Xor Int]
    checkAll("XorT[ListWrapper, String, Int]", OrderLaws[XorT[ListWrapper, String, Int]].partialOrder)
    checkAll("PartialOrder[XorT[ListWrapper, String, Int]]", SerializableTests.serializable(PartialOrder[XorT[ListWrapper, String, Int]]))
  }

  {
    implicit val F = ListWrapper.eqv[String Xor Int]
    checkAll("XorT[ListWrapper, String, Int]", OrderLaws[XorT[ListWrapper, String, Int]].eqv)
    checkAll("Eq[XorT[ListWrapper, String, Int]]", SerializableTests.serializable(Eq[XorT[ListWrapper, String, Int]]))
  }

  {
    implicit val L = ListWrapper.semigroup[String]
    checkAll("XorT[Option, ListWrapper[String], ?]", SemigroupKTests[XorT[Option, ListWrapper[String], ?]].semigroupK[Int])
    checkAll("SemigroupK[XorT[Option, ListWrapper[String], ?]]", SerializableTests.serializable(SemigroupK[XorT[Option, ListWrapper[String], ?]]))
  }

  // make sure that the Monad and Traverse instances don't result in ambiguous
  // Functor instances
  Functor[XorT[List, Int, ?]]

  test("toValidated") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toValidated.map(_.toXor) should === (xort.value)
    }
  }

  test("withValidated") {
    forAll { (xort: XorT[List, String, Int], f: String => Char, g: Int => Double) =>
      xort.withValidated(_.bimap(f, g)) should === (xort.bimap(f, g))
    }
  }

  test("fromXor") {
    forAll { (xor: Xor[String, Int]) =>
      Some(xor.isLeft) should === (XorT.fromXor[Option](xor).isLeft)
    }
  }

  test("isLeft negation of isRight") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.isLeft should === (xort.isRight.map(! _))
    }
  }

  test("double swap is noop") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.swap.swap should === (xort)
    }
  }

  test("swap negates isRight") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.swap.isRight should === (xort.isRight.map(! _))
    }
  }

  test("toOption on Right returns Some") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toOption.isDefined should === (xort.isRight)
    }
  }

  test("toEither preserves isRight") {
    forAll { (xort: XorT[List, String, Int]) =>
      xort.toEither.map(_.isRight) should === (xort.isRight)
    }
  }

  test("recover recovers handled values") {
    val xort = XorT.left[Id, String, Int]("xort")
    xort.recover { case "xort" => 5 }.isRight should === (true)
  }

  test("recover ignores unhandled values") {
    val xort = XorT.left[Id, String, Int]("xort")
    xort.recover { case "notxort" => 5 } should === (xort)
  }

  test("recover ignores the right side") {
    val xort = XorT.right[Id, String, Int](10)
    xort.recover { case "xort" => 5 } should === (xort)
  }

  test("recoverWith recovers handled values") {
    val xort = XorT.left[Id, String, Int]("xort")
    xort.recoverWith { case "xort" => XorT.right[Id, String, Int](5) }.isRight should === (true)
  }

  test("recoverWith ignores unhandled values") {
    val xort = XorT.left[Id, String, Int]("xort")
    xort.recoverWith { case "notxort" => XorT.right[Id, String, Int](5) } should === (xort)
  }

  test("recoverWith ignores the right side") {
    val xort = XorT.right[Id, String, Int](10)
    xort.recoverWith { case "xort" => XorT.right[Id, String, Int](5) } should === (xort)
  }

  test("transform consistent with value.map") {
    forAll { (xort: XorT[List, String, Int], f: String Xor Int => Long Xor Double) =>
      xort.transform(f) should === (XorT(xort.value.map(f)))
    }
  }

  test("subflatMap consistent with value.map+flatMap") {
    forAll { (xort: XorT[List, String, Int], f: Int => String Xor Double) =>
      xort.subflatMap(f) should === (XorT(xort.value.map(_.flatMap(f))))
    }
  }

  test("fold with Id consistent with Xor fold") {
    forAll { (xort: XorT[Id, String, Int], f: String => Long, g: Int => Long) =>
      xort.fold(f, g) should === (xort.value.fold(f, g))
    }
  }

  test("getOrElse with Id consistent with Xor getOrElse") {
    forAll { (xort: XorT[Id, String, Int], i: Int) =>
      xort.getOrElse(i) should === (xort.value.getOrElse(i))
    }
  }

  test("getOrElseF with Id consistent with Xor getOrElse") {
    forAll { (xort: XorT[Id, String, Int], i: Int) =>
      xort.getOrElseF(i) should === (xort.value.getOrElse(i))
    }
  }

  test("orElse with Id consistent with Xor orElse") {
    forAll { (xort: XorT[Id, String, Int], fallback: XorT[Id, String, Int]) =>
      xort.orElse(fallback).value should === (xort.value.orElse(fallback.value))
    }
  }

  test("orElse evaluates effect only once") {
    forAll { (xor: String Xor Int, fallback: XorT[Eval, String, Int]) =>
      var evals = 0
      val xort = (XorT(Eval.always { evals += 1; xor }) orElse fallback)
      xort.value.value
      evals should === (1)
    }
  }

  test("forall with Id consistent with Xor forall") {
    forAll { (xort: XorT[Id, String, Int], f: Int => Boolean) =>
      xort.forall(f) should === (xort.value.forall(f))
    }
  }

  test("exists with Id consistent with Xor exists") {
    forAll { (xort: XorT[Id, String, Int], f: Int => Boolean) =>
      xort.exists(f) should === (xort.value.exists(f))
    }
  }

  test("leftMap with Id consistent with Xor leftMap") {
    forAll { (xort: XorT[Id, String, Int], f: String => Long) =>
      xort.leftMap(f).value should === (xort.value.leftMap(f))
    }
  }

  test("compare with Id consistent with Xor compare") {
    forAll { (x: XorT[Id, String, Int], y: XorT[Id, String, Int]) =>
      x.compare(y) should === (x.value.compare(y.value))
    }
  }

  test("=== with Id consistent with Xor ===") {
    forAll { (x: XorT[Id, String, Int], y: XorT[Id, String, Int]) =>
      x === y should === (x.value === y.value)
    }
  }

  test("traverse with Id consistent with Xor traverse") {
    forAll { (x: XorT[Id, String, Int], f: Int => Option[Long]) =>
      x.traverse(f).map(_.value) should === (x.value.traverse(f))
    }
  }

  test("foldLeft with Id consistent with Xor foldLeft") {
    forAll { (x: XorT[Id, String, Int], l: Long, f: (Long, Int) => Long) =>
      x.foldLeft(l)(f) should === (x.value.foldLeft(l)(f))
    }
  }

  test("foldRight with Id consistent with Xor foldRight") {
    forAll { (x: XorT[Id, String, Int], l: Eval[Long], f: (Int, Eval[Long]) => Eval[Long]) =>
      x.foldRight(l)(f) should === (x.value.foldRight(l)(f))
    }
  }

  test("merge with Id consistent with Xor merge") {
    forAll { (x: XorT[Id, Int, Int]) =>
      x.merge should === (x.value.merge)
    }
  }

  test("to consistent with toOption") {
    forAll { (x: XorT[List, String, Int]) =>
      x.to[Option] should === (x.toOption.value)
    }
  }

  test("toEither consistent with toOption") {
    forAll { (x: XorT[List, String, Int]) =>
      x.toEither.map(_.right.toOption) should === (x.toOption.value)
    }
  }
}
