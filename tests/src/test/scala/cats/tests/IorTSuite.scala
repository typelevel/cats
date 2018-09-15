package cats
package tests

import cats.data.{Ior, IorT}
import cats.kernel.laws.discipline.{
  EqTests,
  MonoidTests,
  SemigroupTests
}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._

class IorTSuite extends CatsSuite {

  checkAll("IorT[Eval, String, ?]", DeferTests[IorT[Eval, String, ?]].defer[Int])

  {
    implicit val F = ListWrapper.functor

    checkAll("IorT[ListWrapper, ?, ?]", BifunctorTests[IorT[ListWrapper, ?, ?]].bifunctor[Int, Int, Int, String, String, String])
    checkAll("Bifunctor[IorT[ListWrapper, ?, ?]]", SerializableTests.serializable(Bifunctor[IorT[ListWrapper, ?, ?]]))

    checkAll("IorT[ListWrapper, Int, ?]", FunctorTests[IorT[ListWrapper, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[IorT[ListWrapper, Int, ?]]", SerializableTests.serializable(Functor[IorT[ListWrapper, Int, ?]]))
  }

  {
    implicit val F = ListWrapper.traverse

    checkAll("IorT[ListWrapper, Int, ?]", TraverseTests[IorT[ListWrapper, Int, ?]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[IorT[ListWrapper, Int, ?]]", SerializableTests.serializable(Traverse[IorT[ListWrapper, Int, ?]]))
  }

  {
    implicit val F = ListWrapper.monad

    checkAll("IorT[ListWrapper, String, Int]", MonadErrorTests[IorT[ListWrapper, String, ?], String].monadError[Int, Int, Int])
    checkAll("MonadError[IorT[List, ?, ?]]", SerializableTests.serializable(MonadError[IorT[ListWrapper, String, ?], String]))
  }

  {
    implicit val F: MonadError[Option, Unit] = catsStdInstancesForOption

    checkAll("IorT[Option, String, String]", MonadErrorTests[IorT[Option, String, ?], Unit].monadError[String, String, String])
    checkAll("MonadError[IorT[Option, ?, ?]]", SerializableTests.serializable(MonadError[IorT[Option, String, ?], Unit]))
  }

  {
    implicit val F = ListWrapper.foldable

    checkAll("IorT[ListWrapper, Int, ?]", FoldableTests[IorT[ListWrapper, Int, ?]].foldable[Int, Int])
    checkAll("Foldable[IorT[ListWrapper, Int, ?]]", SerializableTests.serializable(Foldable[IorT[ListWrapper, Int, ?]]))
  }

  {
    implicit val F = ListWrapper.semigroup[Ior[String, Int]]

    checkAll("IorT[ListWrapper, String, Int]", SemigroupTests[IorT[ListWrapper, String, Int]].semigroup)
    checkAll("Semigroup[IorT[ListWrapper, String, Int]]", SerializableTests.serializable(Semigroup[IorT[ListWrapper, String, Int]]))
  }

  {
    implicit val F = ListWrapper.monoid[Ior[String, Int]]

    checkAll("IorT[ListWrapper, String, Int]", MonoidTests[IorT[ListWrapper, String, Int]].monoid)
    checkAll("Monoid[IorT[ListWrapper, String, Int]]", SerializableTests.serializable(Monoid[IorT[ListWrapper, String, Int]]))
  }

  {
    implicit val F = ListWrapper.eqv[Ior[String, Int]]

    checkAll("IorT[ListWrapper, String, Int]", EqTests[IorT[ListWrapper, String, Int]].eqv)
    checkAll("Eq[IorT[ListWrapper, String, Int]]", SerializableTests.serializable(Eq[IorT[ListWrapper, String, Int]]))
  }

  test("fold with Id consistent with Ior fold") {
    forAll { (iort: IorT[Id, String, Int], fa: String => Long, fb: Int => Long, fab: (String, Int) => Long) =>
      iort.fold(fa, fb, fab) should === (iort.value.fold(fa, fb, fab))
    }
  }

  test("isLeft with Id consistent with Ior isLeft") {
    forAll { (iort: IorT[Id, String, Int]) =>
      iort.isLeft should === (iort.value.isLeft)
    }
  }

  test("isRight with Id consistent with Ior isRight") {
    forAll { (iort: IorT[Id, String, Int]) =>
      iort.isRight should === (iort.value.isRight)
    }
  }

  test("isBoth with Id consistent with Ior isBoth") {
    forAll { (iort: IorT[Id, String, Int]) =>
      iort.isBoth should === (iort.value.isBoth)
    }
  }

  test("isBoth consistent with swap") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.isBoth should === (iort.swap.isBoth)
    }
  }

  test("double swap is noop") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.swap.swap.value should === (iort.value)
    }
  }

  test("getOrElse with Id consistent with Ior getOrElse") {
    forAll { (iort: IorT[Id, String, Int], i: Int) =>
      iort.getOrElse(i) should === (iort.value.getOrElse(i))
    }
  }

  test("getOrElseF with Id consistent with Ior getOrElse") {
    forAll { (iort: IorT[Id, String, Int], i: Int) =>
      iort.getOrElseF(i) should === (iort.value.getOrElse(i))
    }
  }

  test("valueOr with Id consistent with Ior valueOr") {
    forAll { (iort: IorT[Id, String, Int], f: String => Int) =>
      iort.valueOr(f) should === (iort.value.valueOr(f))
    }
  }

  test("forall with Id consistent with Ior forall") {
    forAll { (iort: IorT[Id, String, Int], f: Int => Boolean) =>
      iort.forall(f) should === (iort.value.forall(f))
    }
  }

  test("exists with Id consistent with Ior exists") {
    forAll { (iort: IorT[Id, String, Int], f: Int => Boolean) =>
      iort.exists(f) should === (iort.value.exists(f))
    }
  }

  test("toOption consistent with isLeft") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toOption.isDefined.map(! _) should === (iort.isLeft)
    }
  }

  test("toEither consistent with toOption") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toEither.toOption should === (iort.toOption)
    }
  }

  test("toEither consistent with isLeft") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toEither.isLeft should === (iort.isLeft)
    }
  }

  test("toNested has no loss") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toNested.value should === (iort.value)
    }
  }

  test("toNestedValidated consistent with Ior toValidated") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toNestedValidated.value should === (iort.value.map(_.toValidated))
    }
  }

  test("toValidated consistent with Ior toValidated") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toValidated should === (iort.value.map(_.toValidated))
    }
  }

  test("to consistent with toOption") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.to[Option] should === (iort.toOption.value)
    }
  }

  test("collectRight with List consistent with flattening a to[List]") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.collectRight should === (iort.to[List].flatten)
    }
  }

  test("merge with Id consistent with Ior merge") {
    forAll { (iort: IorT[Id, Int, Int]) =>
      iort.merge should === (iort.value.merge)
    }
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = λ[List ~> Option](_.headOption)
    forAll { (iort: IorT[List, String, Int]) =>
      iort.mapK(f) should === (IorT(f(iort.value)))
    }
  }

  test("leftMap with Id consistent with Ior leftMap") {
    forAll { (iort: IorT[Id, String, Int], f: String => Long) =>
      iort.leftMap(f).value should === (iort.value.leftMap(f))
    }
  }

  test("leftFlatMap consistent with leftMap") {
    forAll { (iort: IorT[List, String, Int], f: String => String) =>
      iort.leftFlatMap(v => IorT.left[Int](List(f(v)))) should ===(iort.leftMap(f))
    }
  }

  test("leftFlatMap consistent with swap and then flatMap") {
    forAll { (iort: IorT[List, String, Int], f: String => IorT[List, String, Int]) =>
      iort.leftFlatMap(f) should ===(iort.swap.flatMap(a => f(a).swap).swap)
    }
  }

  test("leftFlatMapF consistent with leftFlatMap") {
    forAll { (iort: IorT[List, Int, String], f: Int => IorT[List, Int, String])  =>
      iort.leftFlatMapF(f(_).value) should === (iort.leftFlatMap(f))
    }
  }

  test("leftFlatMapF consistent with swap and then flatMapF") {
    forAll { (iort: IorT[List, Int, String], f: Int => List[Ior[Int, String]])  =>
      iort.leftFlatMapF(f) should === (iort.swap.flatMapF(a => f(a).map(_.swap)).swap)
    }
  }

  test("leftSubflatMap consistent with value.map+leftFlatMap") {
    forAll { (iort: IorT[List, Int, String], f: Int => Ior[Double, String]) =>
      iort.leftSubflatMap(f) should === (IorT(iort.value.map(_.leftFlatMap(f))))
    }
  }

  test("leftSubflatMap consistent with swap and then subflatMap") {
    forAll { (iort: IorT[List, Int, String], f: Int => Ior[Double, String]) =>
      iort.leftSubflatMap(f) should === (iort.swap.subflatMap(a => f(a).swap).swap)
    }
  }

  test("leftSemiflatMap consistent with leftMap") {
    forAll { (iort: IorT[List, String, Int], f: String => String) =>
      iort.leftSemiflatMap(v => List(f(v))) should ===(iort.leftMap(f))
    }
  }

  test("leftSemiflatMap consistent with swap and the semiflatMap") {
    forAll { (iort: IorT[List, String, Int], f: String => List[String]) =>
      iort.leftSemiflatMap(f) should ===(iort.swap.semiflatMap(a => f(a)).swap)
    }
  }

  test("transform consistent with value.map") {
    forAll { (iort: IorT[List, String, Int], f: Ior[String, Int] => Ior[Long, Double]) =>
      iort.transform(f) should === (IorT(iort.value.map(f)))
    }
  }

  test("applyAlt with Id consistent with map") {
    forAll { (iort: IorT[Id, String, Int], f: Int => String) =>
      iort.applyAlt(IorT.pure(f)) should === (iort.map(f))
    }
  }

  test("flatMapF consistent with flatMap") {
    forAll { (iort: IorT[List, String, Int], f: Int => IorT[List, String, Int])  =>
      iort.flatMapF(f(_).value) should === (iort.flatMap(f))
    }
  }

  test("subflatMap consistent with value.map+flatMap") {
    forAll { (iort: IorT[List, String, Int], f: Int => Ior[String, Double]) =>
      iort.subflatMap(f) should === (IorT(iort.value.map(_.flatMap(f))))
    }
  }

  test("semiflatMap consistent with value.flatMap+f+right/both") {
    forAll { (iort: IorT[List, String, Int], f: Int => List[Long]) =>
      iort.semiflatMap(f) should === (IorT(iort.value.flatMap {
        case l @ Ior.Left(_) => List(l.asInstanceOf[Ior[String, Long]])
        case Ior.Right(b) => f(b).map(Ior.right)
        case Ior.Both(a, b) => f(b).map(Ior.both(a, _))
      }))
    }
  }

  test("flatTap+pure+right consistent with identity") {
    forAll { (iort: IorT[List, String, Int], f: Int => Long) =>
      iort.flatTap(v => IorT(List(f(v).rightIor[String]))) should === (iort)
    }
  }

  test("flatTapF+pure+right consistent with identity") {
    forAll { (iort: IorT[List, String, Int], f: Int => Long) =>
      iort.flatTapF(v => List(f(v).rightIor[String])) should === (iort)
    }
  }

  test("subflatTap+right consistent with identity") {
    forAll { (iort: IorT[List, String, Int], f: Int => Long) =>
      iort.subflatTap(v => f(v).rightIor[String]) should === (iort)
    }
  }

  test("semiflatTap+pure consistent with identity") {
    forAll { (iort: IorT[List, String, Int], f: Int => Long) =>
      iort.semiflatTap(v => List(f(v))) should === (iort)
    }
  }

  test("leftFlatTap+pure+left consistent with identity") {
    forAll { (iort: IorT[List, Int, String], f: Int => Long) =>
      iort.leftFlatTap(v => IorT(List(f(v).leftIor[String]))) should === (iort)
    }
  }

  test("leftFlatTapF+pure+left consistent with identity") {
    forAll { (iort: IorT[List, Int, String], f: Int => Long) =>
      iort.leftFlatTapF(v => List(f(v).leftIor[String])) should === (iort)
    }
  }

  test("leftSubflatTap+left consistent with identity") {
    forAll { (iort: IorT[List, Int, String], f: Int => Long) =>
      iort.leftSubflatTap(v => f(v).leftIor[String]) should === (iort)
    }
  }

  test("leftSemiflatTap+pure consistent with identity") {
    forAll { (iort: IorT[List, Int, String], f: Int => Long) =>
      iort.leftSemiflatTap(v => List(f(v))) should === (iort)
    }
  }

  test("as consistent with map+const") {
    forAll { (iort: IorT[List, Int, String], d: Long) =>
      iort.as(d) should === (iort.map(_ => d))
    }
  }

  test("leftAs consistent with leftMap+const") {
    forAll { (iort: IorT[List, Int, String], c: Long) =>
      iort.leftAs(c) should === (iort.leftMap(_ => c))
    }
  }

  test("IorT.left with Option isLeft") {
    forAll { (option: Option[String]) =>
      IorT.left[Int](option).isLeft should === (option.map(_ => true))
    }
  }

  test("IorT.leftT isLeft") {
    forAll { (s: String) =>
      IorT.leftT[Option, Int](s).isLeft should === (Some(true))
    }
  }

  test("IorT.right with Option isRight") {
    forAll { (option: Option[Int]) =>
      IorT.right[String](option).isRight should === (option.map(_ => true))
    }
  }

  test("IorT.rightT consistent with IorT.pure") {
    forAll { (i: Int) =>
      IorT.rightT[Option, String](i).value should === (IorT.pure[Option, String](i).value)
    }
  }

  test("IorT.both isBoth with Option consistent with Option zip") {
    forAll { (optionS: Option[String], optionI: Option[Int]) =>
      IorT.both(optionS, optionI).isBoth should === (optionS.zip(optionI).headOption.map(_ => true))
    }
  }

  test("IorT.bothT isBoth") {
    forAll { (s: String, i: Int) =>
      IorT.bothT[Option](s, i).isBoth should === (Some(true))
    }
  }

  test("IorT.pure isRight") {
    forAll { (i: Int) =>
      IorT.rightT[Option, String](i).isRight should === (Some(true))
    }
  }

  test("IorT.liftF consistent with IorT.right") {
    forAll { (option: Option[Int]) =>
      IorT.liftF[Option, String, Int](option).value should === (IorT.right[String](option).value)
    }
  }

  test("IorT.fromIor with Id is noop") {
    forAll { (ior: Ior[String, Int]) =>
      IorT.fromIor[Id](ior).value should === (ior)
    }
  }

  test("IorT.fromEither toEither is noop") {
    forAll { (either: Either[String, Int]) =>
      IorT.fromEither[Id](either).value.toEither should === (either)
    }
  }

  test("IorT.fromEitherF toEither is noop") {
    forAll { (either: Either[String, Int]) =>
      IorT.fromEitherF[Id, String, Int](either).value.toEither should === (either)
    }
  }

  test("IorT.fromOption isLeft consistent with Option isEmpty") {
    forAll { (option: Option[Int], s: String) =>
      IorT.fromOption[Id](option, s).isLeft should === (option.isEmpty)
    }
  }

  test("IorT.fromOptionF isLeft consistent with Option isEmpty") {
    forAll { (option: Option[Int], s: String) =>
      IorT.fromOptionF[Id, String, Int](option, s).isLeft should === (option.isEmpty)
    }
  }

  test("IorT.cond isRight equals test") {
    forAll { (test: Boolean, s: String, i: Int) =>
      val iort = IorT.cond[Id](test, s, i)
      iort.isRight && !iort.isLeft && !iort.isBoth should === (test)
    }
  }

  test("IorT.condF consistent with IorT.right and IorT.left") {
    forAll { (test: Boolean, optionS: Option[String], optionI: Option[Int]) =>
      IorT.condF(test, optionS, optionI) === (if (test) IorT.right(optionS) else IorT.left(optionI))
    }
  }
}
