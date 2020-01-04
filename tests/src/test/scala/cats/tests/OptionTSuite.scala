package cats
package tests

import cats.data.{Const, IdT, OptionT}
import cats.kernel.{Monoid, Semigroup}
import cats.kernel.laws.discipline.{EqTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class OptionTSuite extends CatsSuite {
  implicit val iso = SemigroupalTests.Isomorphisms
    .invariant[OptionT[ListWrapper, *]](OptionT.catsDataFunctorForOptionT(ListWrapper.functor))

  checkAll("OptionT[Eval, *]", DeferTests[OptionT[Eval, *]].defer[Int])
  checkAll("OptionT[Eval, *]", FunctorFilterTests[OptionT[Eval, *]].functorFilter[Int, Int, Int])

  {
    // if a Functor for F is defined
    implicit val F = ListWrapper.functor

    checkAll("OptionT[ListWrapper, *]", FunctorFilterTests[OptionT[ListWrapper, *]].functorFilter[Int, Int, Int])
    checkAll("FunctorFilter[OptionT[ListWrapper, *]]",
             SerializableTests.serializable(FunctorFilter[OptionT[ListWrapper, *]]))

  }

  {
    // if a Traverse for F is defined
    implicit val F = ListWrapper.traverse

    checkAll("OptionT[ListWrapper, *]", TraverseFilterTests[OptionT[ListWrapper, *]].traverseFilter[Int, Int, Int])
    checkAll("TraverseFilter[OptionT[ListWrapper, *]]",
             SerializableTests.serializable(TraverseFilter[OptionT[ListWrapper, *]]))

  }

  {
    implicit val F = ListWrapper.eqv[Option[Int]]

    checkAll("OptionT[ListWrapper, Int]", EqTests[OptionT[ListWrapper, Int]].eqv)
    checkAll("Eq[OptionT[ListWrapper, Int]]", SerializableTests.serializable(Eq[OptionT[ListWrapper, Int]]))
  }

  {
    implicit val F = ListWrapper.partialOrder[Option[Int]]

    checkAll("OptionT[ListWrapper, Int]", PartialOrderTests[OptionT[ListWrapper, Int]].partialOrder)
    checkAll("PartialOrder[OptionT[ListWrapper, Int]]",
             SerializableTests.serializable(PartialOrder[OptionT[ListWrapper, Int]]))

    Eq[OptionT[ListWrapper, Int]]
  }

  {
    implicit val F = ListWrapper.order[Option[Int]]

    checkAll("OptionT[ListWrapper, Int]", OrderTests[OptionT[ListWrapper, Int]].order)
    checkAll("Order[OptionT[ListWrapper, Int]]", SerializableTests.serializable(Order[OptionT[ListWrapper, Int]]))

    PartialOrder[OptionT[ListWrapper, Int]]
    Eq[OptionT[ListWrapper, Int]]
  }

  {
    // F has a Functor
    implicit val F = ListWrapper.functor

    checkAll("OptionT[ListWrapper, Int]", FunctorTests[OptionT[ListWrapper, *]].functor[Int, Int, Int])
    checkAll("Functor[OptionT[ListWrapper, *]]", SerializableTests.serializable(Functor[OptionT[ListWrapper, *]]))
  }

  {
    // F has an Invariant
    implicit val evidence = ListWrapper.invariant
    Invariant[ListWrapper]
    Invariant[OptionT[ListWrapper, *]]

    checkAll("OptionT[ListWrapper, *]", InvariantTests[OptionT[ListWrapper, *]].invariant[Int, Int, Int])
    checkAll("Invariant[OptionT[ListWrapper, *]]", SerializableTests.serializable(Invariant[OptionT[ListWrapper, *]]))
  }

  {
    // F has a Contravariant
    Contravariant[Show]
    Contravariant[OptionT[Show, *]]

    checkAll("OptionT[Show, *]", ContravariantTests[OptionT[Show, *]].contravariant[MiniInt, Int, Boolean])
    checkAll("Contravariant[OptionT[Show, *]]", SerializableTests.serializable(Contravariant[OptionT[Show, *]]))
  }

  {
    // F has a ContravariantMonoidal
    checkAll("OptionT[Const[String, *], Int]",
             ContravariantMonoidalTests[OptionT[Const[String, *], *]].contravariantMonoidal[Int, Int, Int])
    checkAll("ContravariantMonoidal[OptionT[Const[String, *], Int]]",
             SerializableTests.serializable(ContravariantMonoidal[OptionT[Const[String, *], *]]))
  }

  {
    // F has a Monad
    implicit val F = ListWrapper.monad
    implicit val eq0 = OptionT.catsDataEqForOptionT[ListWrapper, Option[Int]]
    implicit val eq1 = OptionT.catsDataEqForOptionT[OptionT[ListWrapper, *], Int](eq0)

    checkAll("OptionT[ListWrapper, Int]", MonadTests[OptionT[ListWrapper, *]].monad[Int, Int, Int])
    checkAll("Monad[OptionT[ListWrapper, *]]", SerializableTests.serializable(Monad[OptionT[ListWrapper, *]]))

    checkAll("OptionT[ListWrapper, Int]", SemigroupKTests[OptionT[ListWrapper, *]].semigroupK[Int])
    checkAll("SemigroupK[OptionT[ListWrapper, *]]", SerializableTests.serializable(SemigroupK[OptionT[ListWrapper, *]]))

    checkAll("OptionT[ListWrapper, Int]", MonoidKTests[OptionT[ListWrapper, *]].monoidK[Int])
    checkAll("MonoidK[OptionT[ListWrapper, *]]", SerializableTests.serializable(MonoidK[OptionT[ListWrapper, *]]))

    Monad[OptionT[ListWrapper, *]]
    FlatMap[OptionT[ListWrapper, *]]
    Applicative[OptionT[ListWrapper, *]]
    Apply[OptionT[ListWrapper, *]]
    Functor[OptionT[ListWrapper, *]]
    MonoidK[OptionT[ListWrapper, *]]
    SemigroupK[OptionT[ListWrapper, *]]
  }

  {
    // F has a MonadError
    type SEither[A] = Either[String, A]

    implicit val monadError = OptionT.catsDataMonadErrorForOptionT[SEither, String]

    checkAll("OptionT[Either[String, *], Int]", MonadErrorTests[OptionT[SEither, *], String].monadError[Int, Int, Int])
    checkAll("MonadError[OptionT[Either[String, *], *]]", SerializableTests.serializable(monadError))

    Monad[OptionT[SEither, *]]
    FlatMap[OptionT[SEither, *]]
    Applicative[OptionT[SEither, *]]
    Apply[OptionT[SEither, *]]
    Functor[OptionT[SEither, *]]
  }

  {
    // F has a Foldable
    implicit val F = ListWrapper.foldable

    checkAll("OptionT[ListWrapper, Int]", FoldableTests[OptionT[ListWrapper, *]].foldable[Int, Int])
    checkAll("Foldable[OptionT[ListWrapper, *]]", SerializableTests.serializable(Foldable[OptionT[ListWrapper, *]]))
  }

  {
    // F has a Traverse
    implicit val F = ListWrapper.traverse

    checkAll("OptionT[ListWrapper, Int] with Option",
             TraverseTests[OptionT[ListWrapper, *]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[OptionT[ListWrapper, *]]", SerializableTests.serializable(Traverse[OptionT[ListWrapper, *]]))

    Foldable[OptionT[ListWrapper, *]]
    Functor[OptionT[ListWrapper, *]]
    Traverse[OptionT[ListWrapper, *]]
  }

  {
    // F[Option[A]] has a monoid
    implicit val FA: Monoid[ListWrapper[Option[Int]]] = ListWrapper.monoid[Option[Int]]

    checkAll("OptionT[ListWrapper, Int]", MonoidTests[OptionT[ListWrapper, Int]].monoid)
    checkAll("Monoid[OptionT[ListWrapper, Int]]", SerializableTests.serializable(Monoid[OptionT[ListWrapper, Int]]))

    Semigroup[OptionT[ListWrapper, Int]]
  }

  {
    // F[Option[A]] has a semigroup
    implicit val FA: Semigroup[ListWrapper[Option[Int]]] = ListWrapper.semigroup[Option[Int]]

    checkAll("OptionT[ListWrapper, Int]", SemigroupTests[OptionT[ListWrapper, Int]].semigroup)
    checkAll("Semigroup[OptionT[ListWrapper, Int]]",
             SerializableTests.serializable(Semigroup[OptionT[ListWrapper, Int]]))
  }

  {
    // MonadError instance where F has a Monad
    implicit val F = ListWrapper.monad
    checkAll("OptionT[ListWrapper, Int]", MonadErrorTests[OptionT[ListWrapper, *], Unit].monadError[Int, Int, Int])
    checkAll("MonadError[OptionT[List, *]]", SerializableTests.serializable(MonadError[OptionT[ListWrapper, *], Unit]))
  }

  test("MonadError[OptionT[F, *], E] instance has higher priority than MonadError[OptionT[F, *], Unit]") {

    def shouldCompile[F[_], E](x: OptionT[F, Int], e: E)(implicit F: MonadError[F, E]): OptionT[F, Int] =
      x.ensure(e)(_ => true)
  }

  test("fold and cata consistent") {
    forAll { (o: OptionT[List, Int], s: String, f: Int => String) =>
      o.fold(s)(f) should ===(o.cata(s, f))
    }
  }

  test("OptionT[Id, A].fold consistent with Option.fold") {
    forAll { (o: Option[Int], s: String, f: Int => String) =>
      o.fold(s)(f) should ===(OptionT[Id, Int](o).fold(s)(f))
    }
  }

  test("OptionT[Id, A].getOrElse consistent with Option.getOrElse") {
    forAll { (o: Option[Int], i: Int) =>
      o.getOrElse(i) should ===(OptionT[Id, Int](o).getOrElse(i))
    }
  }

  test("OptionT[Id, A].getOrElse consistent with Option.getOrElse, with respect to types") {
    forAll { (o: Option[Int]) =>
      o.map(Right.apply).getOrElse(Left("error")) should ===(
        OptionT[Id, Int](o).map(Right.apply).getOrElse("error".asLeft[Int])
      )
    }
  }

  test("OptionT[Id, A].getOrElseF consistent with Option.getOrElse") {
    forAll { (o: Option[Int], i: Int) =>
      o.getOrElse(i) should ===(OptionT[Id, Int](o).getOrElseF(i))
    }
  }

  test("OptionT[Id, A].getOrElseF consistent with Option.getOrElse, with respect to types") {
    forAll { (o: Option[Int]) =>
      o.map(Right.apply).getOrElse(Left("error")) should ===(
        OptionT[Id, Int](o).map(Right.apply).getOrElseF("error".asLeft[Int])
      )
    }
  }
  test("OptionT[Id, A].collect consistent with Option.collect") {
    forAll { (o: Option[Int], f: Int => Option[String]) =>
      val p = Function.unlift(f)
      o.collect(p) should ===(OptionT[Id, Int](o).collect(p).value)
    }
  }

  test("OptionT[Id, A].exists consistent with Option.exists") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.exists(f) should ===(OptionT[Id, Int](o).exists(f))
    }
  }

  test("OptionT[Id, A].filter consistent with Option.filter") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.filter(f) should ===(OptionT[Id, Int](o).filter(f).value)
    }
  }

  test("OptionT[Id, A].withFilter consistent with Option.withFilter") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      (for { x <- o if f(x) } yield x) should ===((for { x <- OptionT[Id, Int](o) if f(x) } yield x).value)
    }
  }

  test("OptionT[Id, A].filterNot consistent with Option.filterNot") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.filterNot(f) should ===(OptionT[Id, Int](o).filterNot(f).value)
    }
  }

  test("OptionT[Id, A].forall consistent with Option.forall") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.forall(f) should ===(OptionT[Id, Int](o).forall(f))
    }
  }

  test("OptionT[Id, A].isDefined consistent with Option.isDefined") {
    forAll { (o: Option[Int]) =>
      o.isDefined should ===(OptionT[Id, Int](o).isDefined)
    }
  }

  test("OptionT[Id, A].isEmpty consistent with Option.isEmpty") {
    forAll { (o: Option[Int]) =>
      o.isEmpty should ===(OptionT[Id, Int](o).isEmpty)
    }
  }

  test("OptionT.when[Id, A] consistent with Option.when") {
    // Option.when is inlined here because it is not available before Scala 2.13
    def when[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (c) Some(a) else None
    forAll { (i: Int, b: Boolean) =>
      OptionT.when[Id, Int](b)(i).value should ===(when(b, i))
    }
  }

  test("OptionT.whenF[Id, A] consistent with Option.when") {
    // Option.when is inlined here because it is not available before Scala 2.13
    def when[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (c) Some(a) else None
    forAll { (i: Int, b: Boolean) =>
      OptionT.whenF[Id, Int](b)(i).value should ===(when(b, i))
    }
  }

  test("OptionT.whenK and OptionT.whenF consistent") {
    forAll { (li: List[Int], b: Boolean) =>
      IdT(li).mapK(OptionT.whenK(b)).value should ===(OptionT.whenF(b)(li))
    }
  }

  test("OptionT.unless[Id, A] consistent with Option.unless") {
    // Option.unless is inlined here because it is not available before Scala 2.13
    def unless[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (!c) Some(a) else None
    forAll { (i: Int, b: Boolean) =>
      OptionT.unless[Id, Int](b)(i).value should ===(unless(b, i))
    }
  }

  test("OptionT.unlessF[Id, A] consistent with Option.unless") {
    // Option.unless is inlined here because it is not available before Scala 2.13
    def unless[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (!c) Some(a) else None
    forAll { (i: Int, b: Boolean) =>
      OptionT.unlessF[Id, Int](b)(i).value should ===(unless(b, i))
    }
  }

  test("OptionT.unlessK and OptionT.unlessF consistent") {
    forAll { (li: List[Int], b: Boolean) =>
      IdT(li).mapK(OptionT.unlessK(b)).value should ===(OptionT.unlessF(b)(li))
    }
  }

  test("orElse and orElseF consistent") {
    forAll { (o1: OptionT[List, Int], o2: OptionT[List, Int]) =>
      o1.orElse(o2) should ===(o1.orElseF(o2.value))
    }
  }

  test("flatMap and flatMapF consistent") {
    forAll { (optionT: OptionT[List, Int], f: Int => OptionT[List, Int]) =>
      optionT.flatMap(f) should ===(optionT.flatMapF(f(_).value))
    }
  }

  test("OptionT[Id, A].toRight consistent with Either.fromOption") {
    forAll { (o: OptionT[Id, Int], s: String) =>
      o.toRight(s).value should ===(Either.fromOption(o.value, s))
    }
  }

  test("toRight consistent with isDefined") {
    forAll { (o: OptionT[List, Int], s: String) =>
      o.toRight(s).isRight should ===(o.isDefined)
    }
  }

  test("toLeft consistent with isDefined") {
    forAll { (o: OptionT[List, Int], s: String) =>
      o.toLeft(s).isLeft should ===(o.isDefined)
    }
  }

  test("isDefined is negation of isEmpty") {
    forAll { (o: OptionT[List, Int]) =>
      o.isDefined should ===(o.isEmpty.map(!_))
    }
  }

  test("fromOption") {
    forAll { (o: Option[Int]) =>
      List(o) should ===(OptionT.fromOption[List](o).value)
    }
  }

  test("liftF") {
    forAll { (xs: List[Int]) =>
      xs.map(Option(_)) should ===(OptionT.liftF(xs).value)
    }
  }

  test("show") {
    val either: Either[String, Option[Int]] = Either.right(Some(1))
    OptionT[Either[String, *], Int](either).show should ===("Right(Some(1))")
  }

  test("none") {
    OptionT.none[List, Int] should ===(OptionT[List, Int](List(None)))
  }

  test("implicit Show[OptionT] instance and explicit show method are consistent") {
    forAll { (optionT: OptionT[List, Int]) =>
      optionT.show should ===(implicitly[Show[OptionT[List, Int]]].show(optionT))
    }
  }

  test("transform consistent with value.map") {
    forAll { (o: OptionT[List, Int], f: Option[Int] => Option[String]) =>
      o.transform(f) should ===(OptionT(o.value.map(f)))
    }
  }

  test("flatTransform consistent with value.flatMap") {
    forAll { (o: OptionT[List, Int], f: Option[Int] => List[Option[String]]) =>
      o.flatTransform(f) should ===(OptionT(o.value.flatMap(f)))
    }
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = λ[List ~> Option](_.headOption)
    forAll { (optiont: OptionT[List, Int]) =>
      optiont.mapK(f) should ===(OptionT(f(optiont.value)))
    }
  }

  test("semiflatMap consistent with value.flatMap+f+pure") {
    forAll { (o: OptionT[List, Int], f: Int => List[String]) =>
      o.semiflatMap(f) should ===(OptionT(o.value.flatMap {
        case None    => List(None)
        case Some(a) => f(a).map(Some(_))
      }))
    }
  }

  test("subflatMap consistent with value.map+flatMap") {
    forAll { (o: OptionT[List, Int], f: Int => Option[String]) =>
      o.subflatMap(f) should ===(OptionT(o.value.map(_.flatMap(f))))
    }
  }

  test("mapFilter consistent with subflatMap") {
    forAll { (o: OptionT[List, Int], f: Int => Option[String]) =>
      o.mapFilter(f) should ===(o.subflatMap(f))
    }
  }

  /**
   * Testing that implicit resolution works. If it compiles, the "test" passes.
   */
  object ImplicitResolution {
    Eq[OptionT[List, Int]]
    PartialOrder[OptionT[List, Int]]
    Order[OptionT[List, Int]]

    Semigroup[OptionT[List, Int]]
    Monoid[OptionT[List, Int]]

    SemigroupK[OptionT[List, *]]
    MonoidK[OptionT[List, *]]

    Functor[OptionT[List, *]]
    Monad[OptionT[List, *]]

    import scala.util.Try
    Functor[OptionT[Try, *]]
    Monad[OptionT[Try, *]]
    MonadError[OptionT[Try, *], Throwable]

    Foldable[OptionT[List, *]]
    Traverse[OptionT[List, *]]

    implicit val T = ListWrapper.traverse
    implicit val M = ListWrapper.monad
    Functor[OptionT[ListWrapper, *]]
  }

}
