package cats
package tests

import cats.kernel.laws.discipline.{EqTests, SerializableTests}
import cats.laws.discipline.ContravariantMonoidalTests
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.{Arbitrary, Cogen}



class EqSuite extends CatsSuite {
    Invariant[Eq]
    Contravariant[Eq]
    Semigroupal[Eq]
    ContravariantSemigroupal[Eq]

    checkAll("Eq[Int]", ContravariantMonoidalTests[Eq].contravariantMonoidal[Int, Int, Int])
    checkAll("ContravariantMonoidal[Eq]", SerializableTests.serializable(ContravariantMonoidal[Eq]))

    case class Something(a: Int, b: Int, c: Int)

    implicit val somethingEq = Eq.allOf[Something](
        Eq.by(_.a), Eq.by(_.b), Eq.by(_.c)
    )

    implicit def arbitrarySomething(implicit i1: Arbitrary[Int], i2: Arbitrary[Int], i3: Arbitrary[Int]): Arbitrary[Something] =
        Arbitrary(for {
            a <- i1.arbitrary
            b <- i2.arbitrary
            c <- i3.arbitrary
        } yield Something(a, b, c))

    implicit val cogenSomething: Cogen[Something] = Cogen(_.hashCode().toLong)

    checkAll("Eq laws", EqTests[Something].eqv)

    test("Eq returned by Eq.allOf yields true on all underlying Eq's true") {
        val sut = Something(1, 2, 3)

        somethingEq.eqv(sut, sut) shouldBe true
    }

    test("Eq returned by Eq.allOf yields false if some of underlying Eq's false") {
        val sut = Something(1, 2, 3)

        somethingEq.eqv(sut, sut.copy(a = 4)) shouldBe false
        somethingEq.eqv(sut, sut.copy(b = 4)) shouldBe false
        somethingEq.eqv(sut, sut.copy(c = 4)) shouldBe false
    }
}
