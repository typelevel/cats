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

import cats.*
import cats.data.{Const, EitherT, Validated, Writer, WriterT}
import cats.kernel.laws.discipline.{EqTests, MonoidTests, SemigroupTests}
import cats.laws.discipline.*
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.option.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*
import org.scalacheck.Test.Parameters
import cats.kernel.laws.discipline.OrderTests

class WriterTSuite extends CatsSuite {
  type Logged[A] = Writer[ListWrapper[Int], A]

  // we have a lot of generated lists of lists in these tests. We have to tell
  // ScalaCheck to calm down a bit so we don't hit memory and test duration
  // issues.
  implicit override val scalaCheckTestParameters: Parameters =
    checkConfiguration.withMaxSize(checkConfiguration.minSize + 5)

  checkAll("WriterT[Eval, Int, *]", DeferTests[WriterT[Eval, Int, *]].defer[Int])
  checkAll("WriterT[List, Int, Int]", EqTests[WriterT[List, Int, Int]].eqv)
  checkAll("Eq[WriterT[List, Int, Int]]", SerializableTests.serializable(Eq[WriterT[List, Int, Int]]))

  checkAll("WriterT[Show, MiniInt, *]",
           ContravariantTests[WriterT[Show, MiniInt, *]].contravariant[MiniInt, Int, Boolean]
  )
  checkAll("Contravariant[WriterT[Show, Int, Int]]",
           SerializableTests.serializable(Contravariant[WriterT[Show, Int, *]])
  )

  // check that this resolves
  Eq[Writer[Int, Int]]

  test("double swap is a noop") {
    forAll { (w: WriterT[List, Int, Int]) =>
      assert(w.swap.swap === w)
    }
  }

  test("reset on pure is a noop") {
    forAll { (i: Int) =>
      val w = Monad[WriterT[List, Int, *]].pure(i)
      assert(w === (w.reset))
    }
  }

  test("reset consistency") {
    forAll { (i: Int, w1: WriterT[Id, Int, Int], w2: WriterT[Id, Int, Int]) =>
      // if the value is the same, everything should be the same
      assert(w1.map(_ => i).reset === (w2.map(_ => i).reset))
    }
  }

  test("tell + written is identity") {
    forAll { (i: Int) =>
      assert(WriterT.tell[Id, Int](i).written === i)
    }
  }

  test("value + value is identity") {
    forAll { (i: Int) =>
      assert(WriterT.value[Id, Int, Int](i).value === i)
    }
  }

  test("valueT + value is identity") {
    forAll { (i: Int) =>
      assert(WriterT.valueT[Id, Int, Int](i).value === i)
    }
  }

  test("value + listen + map(_._1) + value is identity") {
    forAll { (i: Int) =>
      assert(WriterT.value[Id, Int, Int](i).listen.map(_._1).value === i)
    }
  }

  test("tell + listen + map(_._2) + value is identity") {
    forAll { (i: Int) =>
      assert(WriterT.tell[Id, Int](i).listen.map(_._2).value === i)
    }
  }

  test("Writer.pure and WriterT.liftF are consistent") {
    forAll { (i: Int) =>
      val writer: Writer[String, Int] = Writer.value(i)
      val writerT: WriterT[Option, String, Int] = WriterT.liftF(Some(i))
      assert(writer.run.some === (writerT.run))
    }
  }

  test("show") {
    val writerT: WriterT[Id, List[String], String] = WriterT.put("foo")(List("Some log message"))
    assert(writerT.show === "(List(Some log message),foo)")
  }

  test("tell appends to log") {
    val w1: Writer[String, Int] = Writer.value(3)
    val w2 = w1.tell("foo")
    assert(w2 === (Writer("foo", 3)))
    assert(w2.tell("bar") === (Writer("foobar", 3)))
  }

  test("tell instantiates a Writer") {
    assert(Writer.tell("foo").written === "foo")
  }

  test("listen returns a tuple of value and log") {
    val w: Writer[String, Int] = Writer("foo", 3)
    assert(w.listen === (Writer("foo", (3, "foo"))))
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = new (List ~> Option) { def apply[A](a: List[A]): Option[A] = a.headOption }
    forAll { (writert: WriterT[List, String, Int]) =>
      assert(writert.mapK(f) === (WriterT(f(writert.run))))
    }
  }

  {
    // F has a SemigroupK
    implicit val F: SemigroupK[ListWrapper] = ListWrapper.semigroupK

    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             SemigroupKTests[WriterT[ListWrapper, ListWrapper[Int], *]].semigroupK[Int]
    )
    checkAll("SemigroupK[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(SemigroupK[WriterT[ListWrapper, ListWrapper[Int], *]])
    )
  }

  {
    // F has a MonoidK
    implicit val F: MonoidK[ListWrapper] = ListWrapper.monoidK

    SemigroupK[WriterT[ListWrapper, ListWrapper[Int], *]]

    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             MonoidKTests[WriterT[ListWrapper, ListWrapper[Int], *]].monoidK[Int]
    )
    checkAll("MonoidK[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(MonoidK[WriterT[ListWrapper, ListWrapper[Int], *]])
    )
  }

  {
    // F has a Functor and L has no Semigroup
    implicit val F: Functor[ListWrapper] = ListWrapper.functor

    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             FunctorTests[WriterT[ListWrapper, ListWrapper[Int], *]].functor[Int, Int, Int]
    )
    checkAll("Functor[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(Functor[WriterT[ListWrapper, ListWrapper[Int], *]])
    )

    checkAll("WriterT[Listwrapper, Int, *]", CoflatMapTests[WriterT[ListWrapper, Int, *]].coflatMap[Int, Int, Int])
    checkAll("WriterT[ListWrapper, Int, *]", SerializableTests.serializable(CoflatMap[WriterT[ListWrapper, Int, *]]))

    // just making sure this resolves; it's tested above
    Functor[WriterT[Id, ListWrapper[Int], *]]

    Functor[Writer[ListWrapper[Int], *]]

    Functor[Logged]

    checkAll("WriterT[ListWrapper, *, *]",
             BifunctorTests[WriterT[ListWrapper, *, *]].bifunctor[Int, Int, Int, Int, Int, Int]
    )
    checkAll("Bifunctor[WriterT[ListWrapper, *, *]]",
             SerializableTests.serializable(Bifunctor[WriterT[ListWrapper, *, *]])
    )
  }

  implicit val iso: Isomorphisms[WriterT[ListWrapper, ListWrapper[Int], *]] = Isomorphisms
    .invariant[WriterT[ListWrapper, ListWrapper[Int], *]](WriterT.catsDataCoflatMapForWriterT(ListWrapper.functor))

  // We have varying instances available depending on `F` and `L`.
  // We also battle some inference issues with `Id`.
  // Below we go through some gymnastics in order to test both the implicit
  // resolution and the laws of these various instances.
  {
    // F has an Apply and L has a Semigroup
    implicit val F: Apply[ListWrapper] = ListWrapper.applyInstance
    implicit val L: Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], *]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             ApplyTests[WriterT[ListWrapper, ListWrapper[Int], *]].apply[Int, Int, Int]
    )
    checkAll("Apply[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(Apply[WriterT[ListWrapper, ListWrapper[Int], *]])
    )

    Functor[WriterT[Id, ListWrapper[Int], *]]
    Apply[WriterT[Id, ListWrapper[Int], *]]
    CoflatMap[WriterT[Id, ListWrapper[Int], *]]

    Functor[Writer[ListWrapper[Int], *]]
    Apply[Writer[ListWrapper[Int], *]]
    CoflatMap[Writer[ListWrapper[Int], *]]

    Functor[Logged]
    Apply[Logged]
    CoflatMap[Logged]
  }

  {
    // F has a Monad and L has a Semigroup
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val L: Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], *]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], *]]
    CoflatMap[WriterT[ListWrapper, ListWrapper[Int], *]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *] 1",
             FlatMapTests[WriterT[ListWrapper, ListWrapper[Int], *]].flatMap[Int, Int, Int]
    )
    checkAll("FlatMap[WriterT[ListWrapper, ListWrapper[Int], *]] 1",
             SerializableTests.serializable(FlatMap[WriterT[ListWrapper, ListWrapper[Int], *]])
    )

    Functor[WriterT[Id, ListWrapper[Int], *]]
    Apply[WriterT[Id, ListWrapper[Int], *]]
    FlatMap[WriterT[Id, ListWrapper[Int], *]]
    CoflatMap[WriterT[Id, ListWrapper[Int], *]]

    Functor[Writer[ListWrapper[Int], *]]
    Apply[Writer[ListWrapper[Int], *]]
    FlatMap[Writer[ListWrapper[Int], *]]
    CoflatMap[Writer[ListWrapper[Int], *]]

    Functor[Logged]
    Apply[Logged]
    FlatMap[Logged]
    CoflatMap[Logged]
  }
  {
    // F has a FlatMap and L has a Monoid
    implicit val F: FlatMap[ListWrapper] = ListWrapper.flatMap
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], *]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], *]]
    CoflatMap[WriterT[ListWrapper, ListWrapper[Int], *]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *] 2",
             FlatMapTests[WriterT[ListWrapper, ListWrapper[Int], *]].flatMap[Int, Int, Int]
    )
    checkAll("FlatMap[WriterT[ListWrapper, ListWrapper[Int], *]] 2",
             SerializableTests.serializable(FlatMap[WriterT[ListWrapper, ListWrapper[Int], *]])
    )

    Functor[WriterT[Id, ListWrapper[Int], *]]
    Apply[WriterT[Id, ListWrapper[Int], *]]
    FlatMap[WriterT[Id, ListWrapper[Int], *]]
    CoflatMap[WriterT[Id, ListWrapper[Int], *]]

    Functor[Writer[ListWrapper[Int], *]]
    Apply[Writer[ListWrapper[Int], *]]
    FlatMap[Writer[ListWrapper[Int], *]]
    CoflatMap[Writer[ListWrapper[Int], *]]

    Functor[Logged]
    Apply[Logged]
    FlatMap[Logged]
    CoflatMap[Logged]
  }

  {
    // F has an Applicative and L has a Monoid
    implicit val F: Applicative[ListWrapper] = ListWrapper.applicative
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], *]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], *]]
    CoflatMap[WriterT[ListWrapper, ListWrapper[Int], *]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             ApplicativeTests[WriterT[ListWrapper, ListWrapper[Int], *]].applicative[Int, Int, Int]
    )
    checkAll("Applicative[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(Applicative[WriterT[ListWrapper, ListWrapper[Int], *]])
    )

    Functor[WriterT[Id, ListWrapper[Int], *]]
    Apply[WriterT[Id, ListWrapper[Int], *]]
    Applicative[WriterT[Id, ListWrapper[Int], *]]
    CoflatMap[WriterT[Id, ListWrapper[Int], *]]

    Functor[Writer[ListWrapper[Int], *]]
    Apply[Writer[ListWrapper[Int], *]]
    Applicative[Writer[ListWrapper[Int], *]]
    CoflatMap[Writer[ListWrapper[Int], *]]

    Functor[Logged]
    Apply[Logged]
    Applicative[Logged]
    CoflatMap[Logged]
  }

  {
    // F has a Monad and L has a Monoid
    implicit val F: Monad[ListWrapper] = ListWrapper.monad
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], *]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], *]]
    Applicative[WriterT[ListWrapper, ListWrapper[Int], *]]
    FlatMap[WriterT[ListWrapper, ListWrapper[Int], *]]
    CoflatMap[WriterT[ListWrapper, ListWrapper[Int], *]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             MonadTests[WriterT[ListWrapper, ListWrapper[Int], *]].monad[Int, Int, Int]
    )
    checkAll("Monad[WriterT[ListWrapper, ListWrapper[Int], *], List[String]]",
             SerializableTests.serializable(Monad[WriterT[ListWrapper, ListWrapper[Int], *]])
    )

    Functor[WriterT[Id, ListWrapper[Int], *]]
    Apply[WriterT[Id, ListWrapper[Int], *]]
    Applicative[WriterT[Id, ListWrapper[Int], *]]
    FlatMap[WriterT[Id, ListWrapper[Int], *]]
    CoflatMap[WriterT[Id, ListWrapper[Int], *]]
    Monad[WriterT[Id, ListWrapper[Int], *]]

    Functor[Writer[ListWrapper[Int], *]]
    Apply[Writer[ListWrapper[Int], *]]
    Applicative[Writer[ListWrapper[Int], *]]
    FlatMap[Writer[ListWrapper[Int], *]]
    CoflatMap[Writer[ListWrapper[Int], *]]
    Monad[Writer[ListWrapper[Int], *]]

    Functor[Logged]
    Apply[Logged]
    Applicative[Logged]
    FlatMap[Logged]
    CoflatMap[Logged]
    Monad[Logged]
  }

  {
    // F has an Alternative and L has a Monoid
    implicit val F: Alternative[ListWrapper] = ListWrapper.alternative
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], *]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], *]]
    Applicative[WriterT[ListWrapper, ListWrapper[Int], *]]
    Alternative[WriterT[ListWrapper, ListWrapper[Int], *]]
    CoflatMap[WriterT[ListWrapper, ListWrapper[Int], *]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], *]",
             AlternativeTests[WriterT[ListWrapper, ListWrapper[Int], *]].alternative[Int, Int, Int]
    )
    checkAll("Alternative[WriterT[ListWrapper, ListWrapper[Int], *]]",
             SerializableTests.serializable(Alternative[WriterT[ListWrapper, ListWrapper[Int], *]])
    )
  }

  {
    // F[(L, V)] has a monoid
    implicit val FLV: Monoid[ListWrapper[(Int, Int)]] = ListWrapper.monoid[(Int, Int)]

    Monoid[WriterT[ListWrapper, Int, Int]]
    checkAll("WriterT[ListWrapper, Int, Int]", MonoidTests[WriterT[ListWrapper, Int, Int]].monoid)
    checkAll("Monoid[WriterT[ListWrapper, Int, Int]]",
             SerializableTests.serializable(Monoid[WriterT[ListWrapper, Int, Int]])
    )

    Monoid[Writer[Int, Int]]
    checkAll("Writer[Int, Int]", MonoidTests[Writer[Int, Int]].monoid)
  }

  {
    // F[(L, V)] has a semigroup
    implicit val FLV: Semigroup[ListWrapper[(Int, Int)]] = ListWrapper.semigroup[(Int, Int)]

    Semigroup[WriterT[ListWrapper, Int, Int]]
    checkAll("WriterT[ListWrapper, Int, Int]", SemigroupTests[WriterT[ListWrapper, Int, Int]].semigroup)
    checkAll("Semigroup[WriterT[ListWrapper, Int, Int]]",
             SerializableTests.serializable(Semigroup[WriterT[ListWrapper, Int, Int]])
    )

    Semigroup[Writer[Int, Int]]
    checkAll("Writer[Int, Int]", SemigroupTests[Writer[Int, Int]].semigroup)
  }

  {
    // F[(L, V)] has a semigroup
    implicit val FLV: Order[ListWrapper[(Int, Int)]] = ListWrapper.order[(Int, Int)]

    Order[WriterT[ListWrapper, Int, Int]]
    checkAll("WriterT[ListWrapper, Int, Int]", OrderTests[WriterT[ListWrapper, Int, Int]].order)
    checkAll("Order[WriterT[ListWrapper, Int, Int]]",
             SerializableTests.serializable(Order[WriterT[ListWrapper, Int, Int]])
    )

    Order[Writer[Int, Int]]
    checkAll("Writer[Int, Int]", OrderTests[Writer[Int, Int]].order)
  }

  {
    // F has an Applicative and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    implicit val app: Applicative[WriterT[Validated[String, *], ListWrapper[Int], *]] =
      WriterT.catsDataApplicativeForWriterT[Validated[String, *], ListWrapper[Int]]
    implicit val iso: Isomorphisms[WriterT[Validated[String, *], ListWrapper[Int], *]] =
      Isomorphisms.invariant[WriterT[Validated[String, *], ListWrapper[Int], *]]
    implicit def eq1[A: Eq]: Eq[WriterT[Validated[String, *], ListWrapper[Int], A]] =
      WriterT.catsDataEqForWriterT[Validated[String, *], ListWrapper[Int], A]
    implicit val eq2: Eq[EitherT[WriterT[Validated[String, *], ListWrapper[Int], *], String, Int]] =
      EitherT.catsDataEqForEitherT[WriterT[Validated[String, *], ListWrapper[Int], *], String, Int]

    Functor[WriterT[Validated[String, *], ListWrapper[Int], *]]
    Apply[WriterT[Validated[String, *], ListWrapper[Int], *]]
    Applicative[WriterT[Validated[String, *], ListWrapper[Int], *]]

    checkAll("WriterT[Validated[String, *], ListWrapper[Int], *]",
             ApplicativeTests[WriterT[Validated[String, *], ListWrapper[Int], *]].applicative[Int, Int, Int]
    )
    checkAll(
      "Applicative[WriterT[Validated[String, *], ListWrapper[Int], *]]",
      SerializableTests.serializable(Applicative[WriterT[Validated[String, *], ListWrapper[Int], *]])
    )
  }

  {
    // F has an ApplicativeError and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    implicit val appErr: ApplicativeError[WriterT[Validated[String, *], ListWrapper[Int], *], String] =
      WriterT.catsDataApplicativeErrorForWriterT[Validated[String, *], ListWrapper[Int], String]
    implicit val iso: Isomorphisms[WriterT[Validated[String, *], ListWrapper[Int], *]] =
      Isomorphisms.invariant[WriterT[Validated[String, *], ListWrapper[Int], *]]
    checkAll(
      "WriterT[Validated[String, *], ListWrapper[Int], *]",
      ApplicativeErrorTests[WriterT[Validated[String, *], ListWrapper[Int], *], String].applicativeError[Int, Int, Int]
    )
    checkAll(
      "ApplicativeError[WriterT[Validated[String, *], ListWrapper[Int], *], Unit]",
      SerializableTests.serializable(ApplicativeError[WriterT[Validated[String, *], ListWrapper[Int], *], String])
    )
  }

  {
    // F has a MonadError and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    implicit val iso: Isomorphisms[WriterT[Option, ListWrapper[Int], *]] =
      Isomorphisms.invariant[WriterT[Option, ListWrapper[Int], *]]
    implicit val eq0: Eq[EitherT[WriterT[Option, ListWrapper[Int], *], Unit, Int]] =
      EitherT.catsDataEqForEitherT[WriterT[Option, ListWrapper[Int], *], Unit, Int]

    Functor[WriterT[Option, ListWrapper[Int], *]]
    Apply[WriterT[Option, ListWrapper[Int], *]]
    Applicative[WriterT[Option, ListWrapper[Int], *]]
    FlatMap[WriterT[Option, ListWrapper[Int], *]]
    CoflatMap[WriterT[Option, ListWrapper[Int], *]]
    Monad[WriterT[Option, ListWrapper[Int], *]]
    ApplicativeError[WriterT[Option, ListWrapper[Int], *], Unit]

    checkAll("WriterT[Option, ListWrapper[Int], *]",
             MonadErrorTests[WriterT[Option, ListWrapper[Int], *], Unit].monadError[Int, Int, Int]
    )
    checkAll("MonadError[WriterT[Option, ListWrapper[Int], *], Unit]",
             SerializableTests.serializable(MonadError[WriterT[Option, ListWrapper[Int], *], Unit])
    )
  }

  {
    // F has a ContravariantMonoidal
    ContravariantMonoidal[WriterT[Const[String, *], Int, *]]

    checkAll("WriterT[Const[String, *], Int, *]",
             ContravariantMonoidalTests[WriterT[Const[String, *], Int, *]].contravariantMonoidal[Int, Int, Int]
    )
    checkAll("ContravariantMonoidal[WriterT[Const[String, *], Int, *]]",
             SerializableTests.serializable(ContravariantMonoidal[WriterT[Const[String, *], Int, *]])
    )
  }

  {
    // F has an Invariant
    implicit val evidence: Invariant[ListWrapper] = ListWrapper.invariant
    Invariant[ListWrapper]
    Invariant[WriterT[ListWrapper, Int, *]]

    checkAll("WriterT[ListWrapper, Int, *]", InvariantTests[WriterT[ListWrapper, Int, *]].invariant[Int, Int, Int])
    checkAll("Invariant[WriterT[ListWrapper, Int, *]]",
             SerializableTests.serializable(Invariant[WriterT[ListWrapper, Int, *]])
    )
  }

  {
    // F has a Foldable and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    Foldable[Const[String, *]]
    Foldable[WriterT[Const[String, *], ListWrapper[Int], *]]

    checkAll("WriterT[Const[String, *], ListWrapper[Int], *]",
             FoldableTests[WriterT[Const[String, *], ListWrapper[Int], *]].foldable[Int, Int]
    )
    checkAll("Foldable[WriterT[Const[String, *], ListWrapper[Int], *]]",
             SerializableTests.serializable(Foldable[WriterT[Const[String, *], ListWrapper[Int], *]])
    )

    Foldable[Id]
    Foldable[WriterT[Id, ListWrapper[Int], *]]
    Foldable[Writer[ListWrapper[Int], *]]

    checkAll("WriterT[Id, ListWrapper[Int], *]", FoldableTests[WriterT[Id, ListWrapper[Int], *]].foldable[Int, Int])
  }

  {
    // F has a Traverse and L has a Monoid
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]
    Traverse[Const[String, *]]
    Traverse[WriterT[Const[String, *], ListWrapper[Int], *]]

    checkAll("WriterT[Const[String, *], ListWrapper[Int], *]",
             TraverseTests[WriterT[Const[String, *], ListWrapper[Int], *]].traverse[Int, Int, Int, Int, Option, Option]
    )
    checkAll("Traverse[WriterT[Const[String, *], ListWrapper[Int], *]]",
             SerializableTests.serializable(Traverse[WriterT[Const[String, *], ListWrapper[Int], *]])
    )

    Traverse[Id]
    Traverse[WriterT[Id, ListWrapper[Int], *]]
    Traverse[Writer[ListWrapper[Int], *]]

    checkAll("WriterT[Id, ListWrapper[Int], *]",
             TraverseTests[WriterT[Id, ListWrapper[Int], *]].traverse[Int, Int, Int, Int, Option, Option]
    )
  }

  {
    // F has a Comonad and L has a Monoid
    Comonad[WriterT[(String, *), ListWrapper[Int], *]]

    checkAll("WriterT[(String, *), ListWrapper[Int], *]",
             ComonadTests[WriterT[(String, *), ListWrapper[Int], *]].comonad[Int, Int, Int]
    )
    checkAll("Comonad[WriterT[(String, *), ListWrapper[Int], *]]",
             SerializableTests.serializable(Comonad[WriterT[(String, *), ListWrapper[Int], *]])
    )

    Comonad[Id]
    Comonad[WriterT[Id, ListWrapper[Int], *]]
    Comonad[Writer[ListWrapper[Int], *]]

    checkAll("WriterT[Id, ListWrapper[Int], *]", ComonadTests[WriterT[Id, ListWrapper[Int], *]].comonad[Int, Int, Int])
  }

  checkAll("WriterT[Option, Int, *]", CommutativeMonadTests[WriterT[Option, Int, *]].commutativeMonad[Int, Int, Int])
  checkAll("CommutativeMonad[WriterT[Option, Int, *]]",
           SerializableTests.serializable(CommutativeMonad[WriterT[Option, Int, *]])
  )
}
