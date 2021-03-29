package cats.tests

import cats._
import cats.data.{StoreT, Validated}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.syntax.eq._
import org.scalacheck.Prop._
import cats.data.RepresentableStoreT
import org.scalacheck.{Arbitrary, Cogen}

class RepresentableStoreTSuite extends CatsSuite {

  implicit val monoid: Monoid[MiniInt] = MiniInt.miniIntAddition

  implicit val scala2_12_makes_me_sad: Comonad[StoreT[Id, MiniInt, *]] =
    RepresentableStoreT.comonadForStoreT[Id, Function1[MiniInt, *], MiniInt]
  //Like, really, really, really sad
  val a: Arbitrary[Int] = implicitly[Arbitrary[Int]]
  val b: Eq[Int] = Eq[Int]
  val c: Arbitrary[StoreT[Id, MiniInt, Int]] = implicitly[Arbitrary[StoreT[Id, MiniInt, Int]]]
  val d: Cogen[Int] = implicitly[Cogen[Int]]
  val e: Cogen[StoreT[Id, MiniInt, Int]] = implicitly[Cogen[StoreT[Id, MiniInt, Int]]]
  val f: Eq[StoreT[Id, MiniInt, Int]] = Eq[StoreT[Id, MiniInt, Int]]
  val g: Eq[StoreT[Id, MiniInt, StoreT[Id, MiniInt, Int]]] = Eq[StoreT[Id, MiniInt, StoreT[Id, MiniInt, Int]]]
  val h: Eq[StoreT[Id, MiniInt, StoreT[Id, MiniInt, StoreT[Id, MiniInt, Int]]]] =
    Eq[StoreT[Id, MiniInt, StoreT[Id, MiniInt, StoreT[Id, MiniInt, Int]]]]

  checkAll("StoreT[Id, MiniInt, *]",
           ComonadTests[StoreT[Id, MiniInt, *]].comonad[Int, Int, Int](
             a,
             b,
             a,
             b,
             a,
             b,
             c,
             d,
             d,
             d,
             e,
             e,
             f,
             g,
             h,
             f,
             f
           )
  )

  checkAll("Comonad[StoreT[Id, MiniInt, *]]", SerializableTests.serializable(Comonad[StoreT[Id, MiniInt, *]]))

  checkAll("StoreT[Validated[String, *], MiniInt, *]]",
           ApplicativeTests[StoreT[Validated[String, *], MiniInt, *]].applicative[MiniInt, MiniInt, MiniInt]
  )

  checkAll("Comonad[StoreT[Validated[String, *], MiniInt, *]]",
           SerializableTests.serializable(Applicative[StoreT[Validated[String, *], MiniInt, *]])
  )

  test("extract and peek are consistent") {
    forAll { (store: StoreT[Id, String, String]) =>
      assert(store.extract === (store.peek(store.index)))
    }
  }

}
