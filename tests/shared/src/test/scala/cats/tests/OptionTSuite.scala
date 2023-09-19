/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats._
import cats.data.{Const, IdT, OptionT, State}
import cats.kernel.laws.discipline.{EqTests, MonoidTests, OrderTests, PartialOrderTests, SemigroupTests}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.applicative._
import cats.syntax.either._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.monadError._
import cats.syntax.eq._
import org.scalacheck.Prop._

class OptionTSuite extends CatsSuite {
  implicit val iso: Isomorphisms[OptionT[ListWrapper, *]] = Isomorphisms
    .invariant[OptionT[ListWrapper, *]](OptionT.catsDataFunctorForOptionT(ListWrapper.functor))

  checkAll("OptionT[Eval, *]", DeferTests[OptionT[Eval, *]].defer[Int])
  checkAll("OptionT[Eval, *]", FunctorFilterTests[OptionT[Eval, *]].functorFilter[Int, Int, Int])

  {
    // if a Functor for F is defined
    implicit val F: Functor[ListWrapper] = ListWrapper.functor

    checkAll("OptionT[ListWrapper, *]", FunctorFilterTests[OptionT[ListWrapper, *]].functorFilter[Int, Int, Int])
    checkAll("FunctorFilter[OptionT[ListWrapper, *]]",
             SerializableTests.serializable(FunctorFilter[OptionT[ListWrapper, *]])
    )

  }

  {
    // if a Traverse for F is defined
    implicit val F: Traverse[ListWrapper] = ListWrapper.traverse

    checkAll("OptionT[ListWrapper, *]", TraverseFilterTests[OptionT[ListWrapper, *]].traverseFilter[Int, Int, Int])
    checkAll("TraverseFilter[OptionT[ListWrapper, *]]",
             SerializableTests.serializable(TraverseFilter[OptionT[ListWrapper, *]])
    )

  }

  {
    implicit val F: Eq[ListWrapper[Option[Int]]] = ListWrapper.eqv[Option[Int]]

    checkAll("OptionT[ListWrapper, Int]", EqTests[OptionT[ListWrapper, Int]].eqv)
    checkAll("Eq[OptionT[ListWrapper, Int]]", SerializableTests.serializable(Eq[OptionT[ListWrapper, Int]]))
  }

  {
    implicit val F: PartialOrder[ListWrapper[Option[Int]]] = ListWrapper.partialOrder[Option[Int]]

    checkAll("OptionT[ListWrapper, Int]", PartialOrderTests[OptionT[ListWrapper, Int]].partialOrder)
    checkAll("PartialOrder[OptionT[ListWrapper, Int]]",
             SerializableTests.serializable(PartialOrder[OptionT[ListWrapper, Int]])
    )

    Eq[OptionT[ListWrapper, Int]]
  }

  {
    implicit val F: Order[ListWrapper[Option[Int]]] = ListWrapper.order[Option[Int]]

    checkAll("OptionT[ListWrapper, Int]", OrderTests[OptionT[ListWrapper, Int]].order)
    checkAll("Order[OptionT[ListWrapper, Int]]", SerializableTests.serializable(Order[OptionT[ListWrapper, Int]]))

    PartialOrder[OptionT[ListWrapper, Int]]
    Eq[OptionT[ListWrapper, Int]]
  }

  {
    // F has a Functor
    implicit val F: Functor[ListWrapper] = ListWrapper.functor

    checkAll("OptionT[ListWrapper, Int]", FunctorTests[OptionT[ListWrapper, *]].functor[Int, Int, Int])
    checkAll("Functor[OptionT[ListWrapper, *]]", SerializableTests.serializable(Functor[OptionT[ListWrapper, *]]))
  }

  {
    // F has an Invariant
    implicit val evidence: Invariant[ListWrapper] = ListWrapper.invariant
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
             ContravariantMonoidalTests[OptionT[Const[String, *], *]].contravariantMonoidal[Int, Int, Int]
    )
    checkAll("ContravariantMonoidal[OptionT[Const[String, *], Int]]",
             SerializableTests.serializable(ContravariantMonoidal[OptionT[Const[String, *], *]])
    )
  }

  {
    // F has a Monad
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val eq0: Eq[OptionT[ListWrapper, Option[Int]]] = OptionT.catsDataEqForOptionT[ListWrapper, Option[Int]]
    implicit val eq1: Eq[OptionT[OptionT[ListWrapper, *], Int]] =
      OptionT.catsDataEqForOptionT[OptionT[ListWrapper, *], Int](eq0)

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

    implicit val monadError: MonadError[OptionT[SEither, *], String] =
      OptionT.catsDataMonadErrorForOptionT[SEither, String]

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
    implicit val F: Foldable[ListWrapper] = ListWrapper.foldable

    checkAll("OptionT[ListWrapper, Int]", FoldableTests[OptionT[ListWrapper, *]].foldable[Int, Int])
    checkAll("Foldable[OptionT[ListWrapper, *]]", SerializableTests.serializable(Foldable[OptionT[ListWrapper, *]]))
  }

  {
    // F has a Traverse
    implicit val F: Traverse[ListWrapper] = ListWrapper.traverse

    checkAll("OptionT[ListWrapper, Int] with Option",
             TraverseTests[OptionT[ListWrapper, *]].traverse[Int, Int, Int, Int, Option, Option]
    )
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
             SerializableTests.serializable(Semigroup[OptionT[ListWrapper, Int]])
    )
  }

  {
    // MonadError instance where F has a Monad
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    checkAll("OptionT[ListWrapper, Int]", MonadErrorTests[OptionT[ListWrapper, *], Unit].monadError[Int, Int, Int])
    checkAll("MonadError[OptionT[List, *]]", SerializableTests.serializable(MonadError[OptionT[ListWrapper, *], Unit]))
  }

  test("MonadError[OptionT[F, *], E] instance has higher priority than MonadError[OptionT[F, *], Unit]") {

    def shouldCompile[F[_], E](x: OptionT[F, Int], e: E)(implicit F: MonadError[F, E]): OptionT[F, Int] =
      x.ensure(e)(_ => true)
  }

  test("fold and cata consistent") {
    forAll { (o: OptionT[List, Int], s: String, f: Int => String) =>
      assert(o.fold(s)(f) === (o.cata(s, f)))
    }
  }

  test("foldF and cataF consistent") {
    forAll { (o: OptionT[List, Int], s: String, f: Int => List[String]) =>
      assert(o.foldF(List(s))(f) === (o.cataF(List(s), f)))
    }
  }

  test("fold and foldF consistent") {
    forAll { (o: OptionT[List, Int], s: String, f: Int => String) =>
      val f2 = f.andThen(i => List(i))
      assert(o.fold(s)(f) === (o.foldF(List(s))(f2)))
    }
  }

  test("flatTapNone doesn't change the return value") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (optiont: OptionT[TestEffect, Int], f: TestEffect[Int], initial: List[Int]) =>
      assert(optiont.flatTapNone(f).value.runA(initial) === (optiont.value.runA(initial)))
    }
  }

  test("flatTapNone runs the effect") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (optiont: OptionT[TestEffect, Int], f: TestEffect[Int], initial: List[Int]) =>
      assert(
        optiont.flatTapNone(f).value.runS(initial) ===
          optiont.value
            .flatTap {
              case Some(v) => v.pure[TestEffect]
              case None    => f
            }
            .runS(initial)
      )
    }
  }

  test("OptionT[Id, A].fold consistent with Option.fold") {
    forAll { (o: Option[Int], s: String, f: Int => String) =>
      assert(o.fold(s)(f) === (OptionT[Id, Int](o).fold(s)(f)))
    }
  }

  test("OptionT[Id, A].getOrElse consistent with Option.getOrElse") {
    forAll { (o: Option[Int], i: Int) =>
      assert(o.getOrElse(i) === (OptionT[Id, Int](o).getOrElse(i)))
    }
  }

  test("OptionT[Id, A].getOrElse consistent with Option.getOrElse, with respect to types") {
    forAll { (o: Option[Int]) =>
      assert(
        o.map(Right.apply).getOrElse(Left("error")) ===
          OptionT[Id, Int](o).map(Right.apply).getOrElse("error".asLeft[Int])
      )
    }
  }

  test("OptionT[Id, A].getOrElseF consistent with Option.getOrElse") {
    forAll { (o: Option[Int], i: Int) =>
      assert(o.getOrElse(i) === (OptionT[Id, Int](o).getOrElseF(i)))
    }
  }

  test("OptionT[Id, A].getOrElseF consistent with Option.getOrElse, with respect to types") {
    forAll { (o: Option[Int]) =>
      assert(
        o.map(Right.apply).getOrElse(Left("error")) ===
          OptionT[Id, Int](o).map(Right.apply).getOrElseF("error".asLeft[Int])
      )
    }
  }

  test("OptionT[Try, A].getOrRaise consistent with OptionT.getOrElseF(F.raiseError(e))") {
    forAll { (o: Either[String, Option[Int]], error: String) =>
      assertEquals(
        obtained = OptionT[Either[String, *], Int](o).getOrRaise(error),
        expected = OptionT[Either[String, *], Int](o).getOrElseF(Left(error))
      )
    }
  }

  test("OptionT[Id, A].collect consistent with Option.collect") {
    forAll { (o: Option[Int], f: Int => Option[String]) =>
      val p = Function.unlift(f)
      assert(o.collect(p) === (OptionT[Id, Int](o).collect(p).value))
    }
  }

  test("OptionT[Id, A].exists consistent with Option.exists") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      assert(o.exists(f) === (OptionT[Id, Int](o).exists(f)))
    }
  }

  test("OptionT[Id, A].filter consistent with Option.filter") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      assert(o.filter(f) === (OptionT[Id, Int](o).filter(f).value))
    }
  }

  test("OptionT[Id, A].filterF consistent with Option.filter") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      assert(o.filter(f) === (OptionT[Id, Int](o).filterF(f).value))
    }
  }

  test("OptionT[Id, A].withFilter consistent with Option.withFilter") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      assert((for { x <- o if f(x) } yield x) === ((for { x <- OptionT[Id, Int](o) if f(x) } yield x).value))
    }
  }

  test("OptionT[Id, A].filterNot consistent with Option.filterNot") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      assert(o.filterNot(f) === (OptionT[Id, Int](o).filterNot(f).value))
    }
  }

  test("OptionT[Id, A].forall consistent with Option.forall") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      assert(o.forall(f) === (OptionT[Id, Int](o).forall(f)))
    }
  }

  test("OptionT[Id, A].isDefined consistent with Option.isDefined") {
    forAll { (o: Option[Int]) =>
      assert(o.isDefined === (OptionT[Id, Int](o).isDefined))
    }
  }

  test("OptionT[Id, A].isEmpty consistent with Option.isEmpty") {
    forAll { (o: Option[Int]) =>
      assert(o.isEmpty === (OptionT[Id, Int](o).isEmpty))
    }
  }

  test("OptionT.when[Id, A] consistent with Option.when") {
    // Option.when is inlined here because it is not available before Scala 2.13
    def when[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (c) Some(a) else None
    forAll { (i: Int, b: Boolean) =>
      assert(OptionT.when[Id, Int](b)(i).value === (when(b, i)))
    }
  }

  test("OptionT.whenF[Id, A] consistent with Option.when") {
    // Option.when is inlined here because it is not available before Scala 2.13
    def when[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (c) Some(a) else None
    forAll { (i: Int, b: Boolean) =>
      assert(OptionT.whenF[Id, Int](b)(i).value === (when(b, i)))
    }
  }

  test("OptionT.whenM[Id, A] consistent with Option.when") {
    // Option.when is inlined here because it is not available before Scala 2.13
    def when[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (c) Some(a) else None

    forAll { (i: Int, b: Boolean) =>
      assert(OptionT.whenM[Id, Int](b)(i).value === (when(b, i)))
    }
  }

  test("OptionT.whenF and OptionT.whenM consistent") {
    forAll { (li: List[Int], bs: List[Boolean]) =>
      assert(bs.flatMap(OptionT.whenF(_)(li).value) === OptionT.whenM(bs)(li).value)
    }
  }

  test("OptionT.whenK and OptionT.whenF consistent") {
    forAll { (li: List[Int], b: Boolean) =>
      assert(IdT(li).mapK(OptionT.whenK(b)).value === (OptionT.whenF(b)(li)))
    }
  }

  test("OptionT.unless[Id, A] consistent with Option.unless") {
    // Option.unless is inlined here because it is not available before Scala 2.13
    def unless[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (!c) Some(a) else None
    forAll { (i: Int, b: Boolean) =>
      assert(OptionT.unless[Id, Int](b)(i).value === (unless(b, i)))
    }
  }

  test("OptionT.unlessF[Id, A] consistent with Option.unless") {
    // Option.unless is inlined here because it is not available before Scala 2.13
    def unless[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (!c) Some(a) else None
    forAll { (i: Int, b: Boolean) =>
      assert(OptionT.unlessF[Id, Int](b)(i).value === (unless(b, i)))
    }
  }

  test("OptionT.unlessM[Id, A] consistent with Option.unless") {
    // Option.unless is inlined here because it is not available before Scala 2.13
    def unless[A]: (Boolean, A) => Option[A] = (c: Boolean, a: A) => if (!c) Some(a) else None

    forAll { (i: Int, b: Boolean) =>
      assert(OptionT.unlessM[Id, Int](b)(i).value === (unless(b, i)))
    }
  }

  test("OptionT.unlessF and OptionT.unlessM consistent") {
    forAll { (li: List[Int], bs: List[Boolean]) =>
      assert(bs.flatMap(OptionT.unlessF(_)(li).value) === OptionT.unlessM(bs)(li).value)
    }
  }

  test("OptionT.unlessK and OptionT.unlessF consistent") {
    forAll { (li: List[Int], b: Boolean) =>
      assert(IdT(li).mapK(OptionT.unlessK(b)).value === (OptionT.unlessF(b)(li)))
    }
  }

  test("orElse and orElseF consistent") {
    forAll { (o1: OptionT[List, Int], o2: OptionT[List, Int]) =>
      assert(o1.orElse(o2) === (o1.orElseF(o2.value)))
    }
  }

  test("flatMap and flatMapF consistent") {
    forAll { (optionT: OptionT[List, Int], f: Int => OptionT[List, Int]) =>
      assert(optionT.flatMap(f) === (optionT.flatMapF(f(_).value)))
    }
  }

  test("OptionT[Id, A].toRight consistent with Either.fromOption") {
    forAll { (o: OptionT[Id, Int], s: String) =>
      assert(o.toRight(s).value === (Either.fromOption(o.value, s)))
    }
  }

  test("toRight consistent with isDefined") {
    forAll { (o: OptionT[List, Int], s: String) =>
      assert(o.toRight(s).isRight === (o.isDefined))
    }
  }

  test("toRight and toRightF consistent") {
    forAll { (o: OptionT[List, Int], s: String) =>
      assert(o.toRight(s) === (o.toRightF(List(s))))
    }
  }

  test("toLeft consistent with isDefined") {
    forAll { (o: OptionT[List, Int], s: String) =>
      assert(o.toLeft(s).isLeft === (o.isDefined))
    }
  }

  test("toLeft and toLeftF consistent") {
    forAll { (o: OptionT[List, Int], s: String) =>
      assert(o.toLeft(s) === (o.toLeftF(List(s))))
    }
  }

  test("isDefined is negation of isEmpty") {
    forAll { (o: OptionT[List, Int]) =>
      assert(o.isDefined === (o.isEmpty.map(!_)))
    }
  }

  test("fromOption") {
    forAll { (o: Option[Int]) =>
      assert(List(o) === (OptionT.fromOption[List](o).value))
    }
  }

  test("liftF") {
    forAll { (xs: List[Int]) =>
      assert(xs.map(Option(_)) === (OptionT.liftF(xs).value))
    }
  }

  test("show") {
    val either: Either[String, Option[Int]] = Either.right(Some(1))
    assert(OptionT[Either[String, *], Int](either).show === "Right(Some(1))")
  }

  test("none") {
    assert(OptionT.none[List, Int] === (OptionT[List, Int](List(None))))
  }

  test("implicit Show[OptionT] instance and explicit show method are consistent") {
    forAll { (optionT: OptionT[List, Int]) =>
      assert(optionT.show === (implicitly[Show[OptionT[List, Int]]].show(optionT)))
    }
  }

  test("transform consistent with value.map") {
    forAll { (o: OptionT[List, Int], f: Option[Int] => Option[String]) =>
      assert(o.transform(f) === (OptionT(o.value.map(f))))
    }
  }

  test("flatTransform consistent with value.flatMap") {
    forAll { (o: OptionT[List, Int], f: Option[Int] => List[Option[String]]) =>
      assert(o.flatTransform(f) === (OptionT(o.value.flatMap(f))))
    }
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = new (List ~> Option) { def apply[A](a: List[A]): Option[A] = a.headOption }
    forAll { (optiont: OptionT[List, Int]) =>
      assert(optiont.mapK(f) === (OptionT(f(optiont.value))))
    }
  }

  test("semiflatMap consistent with value.flatMap+f+pure") {
    forAll { (o: OptionT[List, Int], f: Int => List[String]) =>
      assert(o.semiflatMap(f) === OptionT(o.value.flatMap {
        case None    => List(None)
        case Some(a) => f(a).map(Some(_))
      }))
    }
  }

  test("semiflatTap consistent with semiflatMap") {
    forAll { (o: OptionT[List, Int], f: Int => List[String]) =>
      assert(o.semiflatMap(v => f(v).as(v)) === (o.semiflatTap(f)))
    }
  }

  test("semiflatTap does not change the return value") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (optiont: OptionT[TestEffect, Int], f: Int => TestEffect[Int], initial: List[Int]) =>
      assert(optiont.semiflatTap(v => f(v)).value.runA(initial) === (optiont.value.runA(initial)))
    }
  }

  test("semiflatTap runs the effect") {
    type TestEffect[A] = State[List[Int], A]
    forAll { (optiont: OptionT[TestEffect, Int], f: Int => TestEffect[Int], initial: List[Int]) =>
      assert(optiont.semiflatTap(v => f(v)).value.runS(initial) === (optiont.semiflatMap(f).value.runS(initial)))
    }
  }

  test("subflatMap consistent with value.map+flatMap") {
    forAll { (o: OptionT[List, Int], f: Int => Option[String]) =>
      assert(o.subflatMap(f) === (OptionT(o.value.map(_.flatMap(f)))))
    }
  }

  test("mapFilter consistent with subflatMap") {
    forAll { (o: OptionT[List, Int], f: Int => Option[String]) =>
      assert(o.mapFilter(f) === (o.subflatMap(f)))
    }
  }

  test("foreachF consistent with foldF") {
    forAll { (o: OptionT[List, Int], f: Int => List[Unit]) =>
      assert(o.foreachF(f) === (o.foldF(List(()))(f)))
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
    MonadThrow[OptionT[Try, *]]

    Foldable[OptionT[List, *]]
    Traverse[OptionT[List, *]]

    implicit val T: Traverse[ListWrapper] = ListWrapper.traverse
    // implicit val M: Monad[ListWrapper] = ListWrapper.monad
    Functor[OptionT[ListWrapper, *]]
  }

}
