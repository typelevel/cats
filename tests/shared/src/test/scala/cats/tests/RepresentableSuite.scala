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
import cats.data.Kleisli
import cats.kernel.Monoid
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.{
  BimonadTests,
  DistributiveTests,
  MiniInt,
  MonadTests,
  RepresentableTests,
  SerializableTests
}
import cats.syntax.representable.*
import org.scalacheck.Arbitrary

class RepresentableSuite extends CatsSuite {

  type Pair[A] = (A, A)

  checkAll("Id[String] <-> Unit => String", RepresentableTests[Id, Unit].representable[String])
  checkAll("Representable[Id]", SerializableTests.serializable(Representable[Id]))

  checkAll("MiniInt => Int <-> MiniInt => Int", RepresentableTests[MiniInt => *, MiniInt].representable[Int])
  checkAll("Representable[String => *]", SerializableTests.serializable(Representable[String => *]))

  checkAll("Pair[String, String] <-> Boolean => String", RepresentableTests[Pair, Boolean].representable[String])
  checkAll("Representable[Pair]", SerializableTests.serializable(Representable[Pair]))

  {
    implicit val rep: Representable.Aux[λ[α => Pair[Pair[α]]], (Boolean, Boolean)] =
      Representable[Pair].compose[Pair]

    checkAll("Pair[Pair[String, String]] <-> (Boolean, Boolean) => String",
             RepresentableTests[λ[α => Pair[Pair[α]]], (Boolean, Boolean)].representable[String]
    )
    checkAll("Representable[Id[Pair]]", SerializableTests.serializable(Representable[λ[α => Id[Pair[α]]]]))
  }

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
             SerializableTests.serializable(Representable[Kleisli[Pair, MiniInt, *]])
    )
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
