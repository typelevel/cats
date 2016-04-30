package cats
package tests

import cats.data.{Writer, WriterT}
import cats.functor.Bifunctor
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._

import cats.kernel.laws.OrderLaws

class WriterTTests extends CatsSuite {
  type Logged[A] = Writer[ListWrapper[Int], A]

  // we have a lot of generated lists of lists in these tests. We have to tell
  // Scalacheck to calm down a bit so we don't hit memory and test duration
  // issues.
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(maxSize = 5, minSuccessful = 20)

  checkAll("WriterT[List, Int, Int]", OrderLaws[WriterT[List, Int, Int]].eqv)
  checkAll("Eq[WriterT[List, Int, Int]]", SerializableTests.serializable(Eq[WriterT[List, Int, Int]]))
  // check that this resolves
  Eq[Writer[Int, Int]]

  test("double swap is a noop"){
    forAll { w: WriterT[List, Int, Int] =>
      w.swap.swap should === (w)
    }
  }

  test("reset on pure is a noop"){
    forAll { i: Int =>
      val w = Monad[WriterT[List, Int, ?]].pure(i)
      w should === (w.reset)
    }
  }

  test("reset consistencey"){
    forAll { (i: Int, w1: WriterT[Id, Int, Int], w2: WriterT[Id, Int, Int]) =>
      // if the value is the same, everything should be the same
      w1.map(_ => i).reset should === (w2.map(_ => i).reset)
    }
  }

  test("tell + written is identity") {
    forAll { (i: Int) =>
      WriterT.tell[Id, Int](i).written should === (i)
    }
  }

  test("value + value is identity") {
    forAll { (i: Int) =>
      WriterT.value[Id, Int, Int](i).value should === (i)
    }
  }

  test("valueT + value is identity") {
    forAll { (i: Int) =>
      WriterT.valueT[Id, Int, Int](i).value should === (i)
    }
  }

  test("show") {
    val writerT: WriterT[Id, List[String], String] = WriterT.put("foo")(List("Some log message"))
    writerT.show should === ("(List(Some log message),foo)")
  }

  {
    // F has a SemigroupK
    implicit val F: SemigroupK[ListWrapper] = ListWrapper.semigroupK

    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", SemigroupKTests[WriterT[ListWrapper, ListWrapper[Int], ?]].semigroupK[Int])
    checkAll("SemigroupK[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(SemigroupK[WriterT[ListWrapper, ListWrapper[Int], ?]]))
  }

  {
    // F has a MonoidK
    implicit val F: MonoidK[ListWrapper] = ListWrapper.monoidK

    SemigroupK[WriterT[ListWrapper, ListWrapper[Int], ?]]

    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", MonoidKTests[WriterT[ListWrapper, ListWrapper[Int], ?]].monoidK[Int])
    checkAll("MonoidK[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(MonoidK[WriterT[ListWrapper, ListWrapper[Int], ?]]))
  }

  {
    // F has a Functor and L has no Semigroup
    implicit val F: Functor[ListWrapper] = ListWrapper.functor

    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", FunctorTests[WriterT[ListWrapper, ListWrapper[Int], ?]].functor[Int, Int, Int])
    checkAll("Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]))

    // just making sure this resolves; it's tested above
    Functor[WriterT[Id, ListWrapper[Int], ?]]

    Functor[Writer[ListWrapper[Int], ?]]

    Functor[Logged]

    checkAll("WriterT[ListWrapper, ?, ?]", BifunctorTests[WriterT[ListWrapper, ?, ?]].bifunctor[Int, Int, Int, Int, Int, Int])
    checkAll("Bifunctor[WriterT[ListWrapper, ?, ?]]", SerializableTests.serializable(Bifunctor[WriterT[ListWrapper, ?, ?]]))
  }

  implicit val iso = CartesianTests.Isomorphisms.invariant[WriterT[ListWrapper, ListWrapper[Int], ?]](WriterT.writerTFunctor(ListWrapper.functor))

  // We have varying instances available depending on `F` and `L`.
  // We also battle some inference issues with `Id`.
  // Below we go through some gymnastics in order to test both the implicit
  // resolution and the laws of these various instances.
  {
    // F has an Apply and L has a Semigroup
    implicit val F: Apply[ListWrapper] = ListWrapper.monadCombine
    implicit val L: Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", ApplyTests[WriterT[ListWrapper, ListWrapper[Int], ?]].apply[Int, Int, Int])
    checkAll("Apply[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(Apply[WriterT[ListWrapper, ListWrapper[Int], ?]]))

    Functor[WriterT[Id, ListWrapper[Int], ?]]
    Apply[WriterT[Id, ListWrapper[Int], ?]]

    Functor[Writer[ListWrapper[Int], ?]]
    Apply[Writer[ListWrapper[Int], ?]]

    Functor[Logged]
    Apply[Logged]
  }

  {
    // F has a FlatMap and L has a Semigroup
    implicit val F: FlatMap[ListWrapper] = ListWrapper.monadCombine
    implicit val L: Semigroup[ListWrapper[Int]] = ListWrapper.semigroup[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], ?]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", FlatMapTests[WriterT[ListWrapper, ListWrapper[Int], ?]].flatMap[Int, Int, Int])
    checkAll("FlatMap[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(FlatMap[WriterT[ListWrapper, ListWrapper[Int], ?]]))

    Functor[WriterT[Id, ListWrapper[Int], ?]]
    Apply[WriterT[Id, ListWrapper[Int], ?]]
    FlatMap[WriterT[Id, ListWrapper[Int], ?]]

    Functor[Writer[ListWrapper[Int], ?]]
    Apply[Writer[ListWrapper[Int], ?]]
    FlatMap[Writer[ListWrapper[Int], ?]]

    Functor[Logged]
    Apply[Logged]
    FlatMap[Logged]
  }

  {
    // F has an Applicative and L has a Monoid
    implicit val F: Applicative[ListWrapper] = ListWrapper.monadCombine
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], ?]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", ApplicativeTests[WriterT[ListWrapper, ListWrapper[Int], ?]].applicative[Int, Int, Int])
    checkAll("Applicative[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(Applicative[WriterT[ListWrapper, ListWrapper[Int], ?]]))

    Functor[WriterT[Id, ListWrapper[Int], ?]]
    Apply[WriterT[Id, ListWrapper[Int], ?]]
    Applicative[WriterT[Id, ListWrapper[Int], ?]]

    Functor[Writer[ListWrapper[Int], ?]]
    Apply[Writer[ListWrapper[Int], ?]]
    Applicative[Writer[ListWrapper[Int], ?]]

    Functor[Logged]
    Apply[Logged]
    Applicative[Logged]
  }

  {
    // F has a Monad and L has a Monoid
    implicit val F: Monad[ListWrapper] = ListWrapper.monadCombine
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Applicative[WriterT[ListWrapper, ListWrapper[Int], ?]]
    FlatMap[WriterT[ListWrapper, ListWrapper[Int], ?]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", MonadWriterTests[WriterT[ListWrapper, ListWrapper[Int], ?], ListWrapper[Int]].monadWriter[Int, Int, Int])
    checkAll("MonadWriter[WriterT[ListWrapper, ListWrapper[Int], ?], List[String]]", SerializableTests.serializable(MonadWriter[WriterT[ListWrapper, ListWrapper[Int], ?], ListWrapper[Int]]))

    Functor[WriterT[Id, ListWrapper[Int], ?]]
    Apply[WriterT[Id, ListWrapper[Int], ?]]
    Applicative[WriterT[Id, ListWrapper[Int], ?]]
    FlatMap[WriterT[Id, ListWrapper[Int], ?]]
    Monad[WriterT[Id, ListWrapper[Int], ?]]

    Functor[Writer[ListWrapper[Int], ?]]
    Apply[Writer[ListWrapper[Int], ?]]
    Applicative[Writer[ListWrapper[Int], ?]]
    FlatMap[Writer[ListWrapper[Int], ?]]
    Monad[Writer[ListWrapper[Int], ?]]

    Functor[Logged]
    Apply[Logged]
    Applicative[Logged]
    FlatMap[Logged]
    Monad[Logged]
  }

  {
    // F has an Alternative and L has a Monoid
    implicit val F: Alternative[ListWrapper] = ListWrapper.alternative
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Applicative[WriterT[ListWrapper, ListWrapper[Int], ?]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", AlternativeTests[WriterT[ListWrapper, ListWrapper[Int], ?]].alternative[Int, Int, Int])
    checkAll("Alternative[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(Alternative[WriterT[ListWrapper, ListWrapper[Int], ?]]))
  }

  {
    // F has a MonadFilter and L has a Monoid
    implicit val F: MonadFilter[ListWrapper] = ListWrapper.monadFilter
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Applicative[WriterT[ListWrapper, ListWrapper[Int], ?]]
    FlatMap[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Monad[WriterT[ListWrapper, ListWrapper[Int], ?]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", MonadFilterTests[WriterT[ListWrapper, ListWrapper[Int], ?]].monadFilter[Int, Int, Int])
    checkAll("MonadFilter[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(MonadFilter[WriterT[ListWrapper, ListWrapper[Int], ?]]))
  }

  {
    // F has a MonadCombine and L has a Monoid
    implicit val F: MonadCombine[ListWrapper] = ListWrapper.monadCombine
    implicit val L: Monoid[ListWrapper[Int]] = ListWrapper.monoid[Int]

    Functor[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Apply[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Applicative[WriterT[ListWrapper, ListWrapper[Int], ?]]
    FlatMap[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Monad[WriterT[ListWrapper, ListWrapper[Int], ?]]
    MonadFilter[WriterT[ListWrapper, ListWrapper[Int], ?]]
    Alternative[WriterT[ListWrapper, ListWrapper[Int], ?]]
    SemigroupK[WriterT[ListWrapper, ListWrapper[Int], ?]]
    MonoidK[WriterT[ListWrapper, ListWrapper[Int], ?]]
    checkAll("WriterT[ListWrapper, ListWrapper[Int], ?]", MonadCombineTests[WriterT[ListWrapper, ListWrapper[Int], ?]].monadCombine[Int, Int, Int])
    checkAll("MonadCombine[WriterT[ListWrapper, ListWrapper[Int], ?]]", SerializableTests.serializable(MonadCombine[WriterT[ListWrapper, ListWrapper[Int], ?]]))
  }
}
