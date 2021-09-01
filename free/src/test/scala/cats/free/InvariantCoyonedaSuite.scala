package cats.free

import cats.~>
import cats.arrow.FunctionK
import cats.Invariant
import cats.Semigroup
import cats.instances.all._
import cats.kernel.Eq
import cats.laws.discipline.{InvariantTests, SerializableTests}
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary}
import cats.syntax.eq._
import cats.syntax.semigroup._
import org.scalacheck.Prop._

class InvariantCoyonedaSuite extends CatsSuite {

  // If we can generate functions we can generate an interesting InvariantCoyoneda.
  implicit def invariantCoyonedaArbitrary[F[_], A: Semigroup, T](implicit
    F: Arbitrary[(A, A) => A]
  ): Arbitrary[InvariantCoyoneda[Semigroup, A]] =
    Arbitrary(
      F.arbitrary.map(f =>
        InvariantCoyoneda.lift[Semigroup, A](Semigroup.instance { (x, y) =>
          f(x, y) |+| f(y, x)
        })
      )
    )

  // We can't really test that semigroups are equal but we can try it with a bunch of test data.
  implicit def invariantCoyonedaEq[A: Arbitrary: Eq]: Eq[InvariantCoyoneda[Semigroup, A]] =
    new Eq[InvariantCoyoneda[Semigroup, A]] {
      def eqv(cca: InvariantCoyoneda[Semigroup, A], ccb: InvariantCoyoneda[Semigroup, A]): Boolean =
        Arbitrary.arbitrary[List[(A, A)]].sample.get.forall { case (x, y) =>
          cca.run.combine(x, y) == ccb.run.combine(x, y)
        }
    }

  checkAll("InvariantCoyoneda[Semigroup, Int]",
           InvariantTests[InvariantCoyoneda[Semigroup, *]].invariant[Int, Int, Int]
  )
  checkAll("Invariant[InvariantCoyoneda[Option, *]]",
           SerializableTests.serializable(Invariant[InvariantCoyoneda[Option, *]])
  )

  test("mapK and run is same as applying natural trans") {
    forAll { (x: Option[Int], y: Option[Int]) =>
      val nt = new (Semigroup ~> Semigroup) {
        def apply[A](sg: Semigroup[A]): Semigroup[A] = sg.reverse
      }
      val c = InvariantCoyoneda.lift[Semigroup, Option[Int]](Semigroup[Option[Int]])
      c.mapK[Semigroup](nt).run.combine(x, y) === nt(Semigroup[Option[Int]]).combine(x, y)
    }
  }

  test("stack-safe imapmap") {
    def loop(n: Int, acc: InvariantCoyoneda[Semigroup, Int]): InvariantCoyoneda[Semigroup, Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.imap((_: Int) + 1)((_: Int) - 1))
    loop(20000, InvariantCoyoneda.lift[Semigroup, Int](Semigroup[Int])).run.combine(10, 11)
  }

  test("run, foldMap consistent") {
    forAll {
      (
        c: InvariantCoyoneda[Semigroup, String],
        f1: String => Byte,
        g1: Byte => String,
        f2: Byte => Float,
        g2: Float => Byte,
        s1: Float,
        s2: Float
      ) =>
        val cʹ = c.imap(f1)(g1).imap(f2)(g2) // just to ensure there's some structure
        val h = cʹ.foldMap[Semigroup](FunctionK.id[Semigroup])
        cʹ.run.combine(s1, s2) === h.combine(s1, s2)
    }
  }

}
