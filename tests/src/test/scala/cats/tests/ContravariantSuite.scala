package cats.tests

import cats.{Contravariant, ContravariantMonoidal, ContravariantSemigroupal}
import cats.data.Const
import cats.kernel.{Eq, Monoid, Semigroup}
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.{ContravariantMonoidalTests, ExhaustiveCheck, MiniInt}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.{Arbitrary, Cogen}
import cats.ContravariantChoosable
import cats.laws.discipline.ContravariantChoosableTests

class ContravariantSuite extends CatsSuite {

  test("narrow equals contramap(identity)") {
    implicit val constInst: Contravariant[Const[Int, *]] = Const.catsDataContravariantForConst[Int]
    forAll { (i: Int) =>
      val const: Const[Int, Option[Int]] = Const[Int, Option[Int]](i)
      val narrowed: Const[Int, Some[Int]] = constInst.narrow[Option[Int], Some[Int]](const)
      narrowed should ===(constInst.contramap(const)(identity[Option[Int]](_: Some[Int])))
      assert(narrowed eq const)
    }
  }

  case class Predicate[A](run: A => Boolean)

  implicit val contravariantMonoidalPredicate: ContravariantMonoidal[Predicate] =
    new ContravariantMonoidal[Predicate] {
      def unit: Predicate[Unit] = Predicate[Unit](Function.const(true))
      def product[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[(A, B)] =
        Predicate(x => fa.run(x._1) && fb.run(x._2))
      def contramap[A, B](fa: Predicate[A])(f: B => A): Predicate[B] =
        Predicate(x => fa.run(f(x)))
    }

  implicit val contravariantChoosablePredicate: ContravariantChoosable[Predicate] =
    new ContravariantChoosable[Predicate] {
      def choice[A, B](fa: Predicate[A], fb: Predicate[B]): Predicate[Either[A, B]] =
        Predicate {
          case Left(a)  => fa.run(a)
          case Right(b) => fb.run(b)
        }

      def zero: Predicate[Nothing] = Predicate(Function.const(false))

      def contravariant: Contravariant[Predicate] = contravariantMonoidalPredicate

    }

  implicit def eqPredicate[A: ExhaustiveCheck]: Eq[Predicate[A]] =
    Eq.by[Predicate[A], A => Boolean](_.run)

  implicit def arbPredicate[A: Cogen]: Arbitrary[Predicate[A]] =
    Arbitrary(implicitly[Arbitrary[A => Boolean]].arbitrary.map(f => Predicate(f)))

  checkAll("ContravariantMonoidal[Predicate]",
           ContravariantMonoidalTests[Predicate].contravariantMonoidal[Boolean, Boolean, Boolean]
  )
  checkAll("ContravariantChoosable[Predicate]",
           ContravariantChoosableTests[Predicate].contravariantChoosable[Boolean, Boolean, Boolean]
  )

  {
    implicit val predicateMonoid: Monoid[Predicate[MiniInt]] = ContravariantMonoidal.monoid[Predicate, MiniInt]
    checkAll("ContravariantMonoidal[Predicate].monoid", MonoidTests[Predicate[MiniInt]].monoid)
  }
  {
    implicit val predicateSemigroup: Semigroup[Predicate[MiniInt]] =
      ContravariantSemigroupal.semigroup[Predicate, MiniInt]
    checkAll("ContravariantSemigroupal[Predicate].semigroup", SemigroupTests[Predicate[MiniInt]].semigroup)
  }

}
