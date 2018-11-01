package cats.tests

import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline.{BimonadTests, MonadTests, RepresentableTests, SerializableTests}
import cats.{Bimonad, Eq, Eval, Id, Representable}
import org.scalacheck.Arbitrary
import cats.data.Kleisli

class RepresentableSuite extends CatsSuite {

  type Pair[A] = (A, A)

  checkAll("Id[String] <-> Unit => String", RepresentableTests[Id, Unit].representable[String])
  checkAll("Representable[Id]", SerializableTests.serializable(Representable[Id]))

  checkAll("String => Int <-> String => Int", RepresentableTests[String => ?, String].representable[Int])
  checkAll("Representable[String => ?]", SerializableTests.serializable(Representable[String => ?]))

  checkAll("Pair[String, String] <-> Boolean => String", RepresentableTests[Pair, Boolean].representable[String])
  checkAll("Representable[Pair]", SerializableTests.serializable(Representable[Pair]))

  checkAll("Eval[Int] <-> Unit => Int", RepresentableTests[Eval, Unit].representable[Int])
  checkAll("Representable[Eval]", SerializableTests.serializable(Representable[Eval]))

  {
    implicit val representableKleisliPair = Kleisli.catsDataRepresentableForKleisli[Pair, Boolean, String]

    implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
      Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

    checkAll(
      "Kleisli[Pair, String, Int] <-> (String, Boolean) => Int",
      // Have to summon all implicits using 'implicitly' otherwise we get a diverging implicits error
      RepresentableTests[Kleisli[Pair, String, ?], (String, Boolean)].representable[Int](
        implicitly[Arbitrary[Int]],
        implicitly[Arbitrary[Kleisli[Pair, String, Int]]],
        implicitly[Arbitrary[(String, Boolean)]],
        implicitly[Arbitrary[((String, Boolean)) => Int]],
        implicitly[Eq[Kleisli[Pair, String, Int]]],
        implicitly[Eq[Int]]
      )
    )

    checkAll("Representable[Kleisli[Pair, String, ?]]",
             SerializableTests.serializable(Representable[Kleisli[Pair, String, ?]]))
  }

  {
    implicit val andMonoid = new cats.Monoid[Boolean] {
      def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    implicit val isoPair = Isomorphisms.invariant[Pair]
    implicit val bimonadInstance = Representable.bimonad[Pair, Boolean]
    checkAll("Pair[Int]", BimonadTests[Pair].bimonad[Int, Int, Int])
    checkAll("Bimonad[Pair]", SerializableTests.serializable(Bimonad[Pair]))
  }

  {
    implicit val isoFun1: Isomorphisms[String => ?] =
      Isomorphisms.invariant[String => ?] //the monadInstance below made a conflict to resolve this one.
      {
        implicit val monadInstance = Representable.monad[String => ?]
        checkAll("String => ?", MonadTests[String => ?].monad[String, String, String])
      }
  }

  // Syntax tests. If it compiles is "passes"
  {
    // Pair
    val pair: Pair[Int] = (3, 5)
    val indexedPair: Boolean => Int = pair.index
    val tabulatedPair = indexedPair.tabulate[Pair]

    // Function
    val function: String => Int = _.length
    val indexedFunction = function.index
    val tabulatedFunction = indexedFunction.tabulate
  }
}
