package cats.tests

import cats.{Bimonad, Distributive, Eq, Eval, Id, Monad, Representable}
import cats.data.Kleisli
import cats.instances.all._
import cats.kernel.Monoid
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import cats.laws.discipline.{
  BimonadTests,
  DistributiveTests,
  MiniInt,
  MonadTests,
  RepresentableTests,
  SerializableTests
}
import cats.syntax.all._
import org.scalacheck.Arbitrary

class RepresentableSuite extends CatsSuite {

  type Pair[A] = (A, A)

  checkAll("Id[String] <-> Unit => String", RepresentableTests[Id, Unit].representable[String])
  checkAll("Representable[Id]", SerializableTests.serializable(Representable[Id]))

  checkAll("MiniInt => Int <-> MiniInt => Int", RepresentableTests[MiniInt => *, MiniInt].representable[Int])
  checkAll("Representable[String => *]", SerializableTests.serializable(Representable[String => *]))

  checkAll("Pair[String, String] <-> Boolean => String", RepresentableTests[Pair, Boolean].representable[String])
  checkAll("Representable[Pair]", SerializableTests.serializable(Representable[Pair]))

  checkAll("Eval[Int] <-> Unit => Int", RepresentableTests[Eval, Unit].representable[Int])
  checkAll("Representable[Eval]", SerializableTests.serializable(Representable[Eval]))

  {
    implicit val representableKleisliPair: Representable.Aux[Kleisli[Pair, MiniInt, *], (MiniInt, Boolean)] =
      Kleisli.catsDataRepresentableForKleisli[Pair, Boolean, MiniInt]

    implicit def kleisliEq[F[_], A, B](implicit ev: Eq[A => F[B]]): Eq[Kleisli[F, A, B]] =
      Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

    checkAll(
      "Kleisli[Pair, MiniInt, Int] <-> (MiniInt, Boolean) => Int",
      // Have to summon all implicits using 'implicitly' otherwise we get a diverging implicits error
      RepresentableTests[Kleisli[Pair, MiniInt, *], (MiniInt, Boolean)].representable[Int](
        implicitly[Arbitrary[Int]],
        implicitly[Arbitrary[Kleisli[Pair, MiniInt, Int]]],
        implicitly[Arbitrary[(MiniInt, Boolean)]],
        implicitly[Arbitrary[((MiniInt, Boolean)) => Int]],
        implicitly[Eq[Kleisli[Pair, MiniInt, Int]]],
        implicitly[Eq[Int]]
      )
    )

    checkAll("Representable[Kleisli[Pair, MiniInt, *]]",
             SerializableTests.serializable(Representable[Kleisli[Pair, MiniInt, *]]))
  }

  val reprPair = Representable[Pair]
  val reprMiniIntFunc = Representable[MiniInt => *]
  val isoPair: Isomorphisms[Pair] = Isomorphisms.invariant[Pair]
  val isoMiniIntFunc: Isomorphisms[MiniInt => *] = Isomorphisms.invariant[MiniInt => *]

  {
    implicit val andMonoid: Monoid[Boolean] = new Monoid[Boolean] {
      def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    implicit val isoPairInstance: Isomorphisms[Pair] = isoPair
    implicit val bimonadInstance: Bimonad[Pair] = Representable.bimonad[Pair, Boolean](reprPair, Monoid[Boolean])
    checkAll("Pair[Int]", BimonadTests[Pair].bimonad[Int, Int, Int])
    checkAll("Bimonad[Pair]", SerializableTests.serializable(Bimonad[Pair]))
  }

  {
    implicit val isoFun1: Isomorphisms[MiniInt => *] = isoMiniIntFunc
    implicit val monadInstance: Monad[MiniInt => *] = Representable.monad[MiniInt => *](reprMiniIntFunc)
    checkAll("MiniInt => *", MonadTests[MiniInt => *].monad[String, String, String])
  }

  {
    implicit val distributiveInstance: Distributive[Pair] = Representable.distributive[Pair](reprPair)
    checkAll("Pair[Int]", DistributiveTests[Pair].distributive[Int, Int, Int, Option, MiniInt => *])
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
