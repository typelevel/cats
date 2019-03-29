package cats.tests

import cats.Comonad
import cats.laws.discipline.{ComonadTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.data.RepresentableStore
import cats.data.Store

class RepresentableStoreSuite extends CatsSuite {

  // Note: The Eq instance for Function1 causes this to run excessively long, and timeout the travis build
  // checkAll("Comonad[Store[String, ?]]", ComonadTests[Store[String, ?]].comonad[Int, Int, Int])

  {
    implicit val pairComonad = RepresentableStore.catsDataRepresentableStoreComonad[λ[P => (P, P)], Boolean]
    implicit val arbStore = catsLawsArbitraryForRepresentableStore[λ[P => (P, P)], Boolean, Int]
    implicit val cogenStore = catsLawsCogenForRepresentableStore[λ[P => (P, P)], Boolean, Int]
    implicit val eqStore = cats.laws.discipline.eq.catsLawsEqForRepresentableStore[λ[P => (P, P)], Boolean, Int]
    implicit val eqStoreStore = cats.laws.discipline.eq
      .catsLawsEqForRepresentableStore[λ[P => (P, P)], Boolean, RepresentableStore[λ[P => (P, P)], Boolean, Int]]
    implicit val eqStoreStoreStore =
      cats.laws.discipline.eq.catsLawsEqForRepresentableStore[λ[P => (P, P)], Boolean, RepresentableStore[λ[
        P => (P, P)
      ], Boolean, RepresentableStore[λ[P => (P, P)], Boolean, Int]]]
    checkAll("Comonad[RepresentableStore[λ[P => (P, P)], Boolean, ?]]",
             ComonadTests[RepresentableStore[λ[P => (P, P)], Boolean, ?]].comonad[Int, Int, Int])

    checkAll("Comonad[RepresentableStore[λ[P => (P, P)], Boolean, ?]]",
             SerializableTests.serializable(Comonad[RepresentableStore[λ[P => (P, P)], Boolean, ?]]))
  }

  test("extract and peek are consistent") {
    forAll { (store: Store[String, String]) =>
      store.extract should ===(store.peek(store.index))
    }
  }

  test("use store alias constructor") {
    forAll { (f: String => Int, s: String) =>
      val store = Store(f, s)
      store.extract should ===(f(s))
    }
  }
}
