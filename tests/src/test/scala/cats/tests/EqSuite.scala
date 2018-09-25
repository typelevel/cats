package cats
package tests

import cats.kernel.laws.discipline.SerializableTests
import cats.laws.discipline.ContravariantMonoidalTests
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._



class EqSuite extends CatsSuite {
    Invariant[Eq]
    Contravariant[Eq]
    Semigroupal[Eq]
    ContravariantSemigroupal[Eq]

    checkAll("Eq[Int]", ContravariantMonoidalTests[Eq].contravariantMonoidal[Int, Int, Int])
    checkAll("ContravariantMonoidal[Eq]", SerializableTests.serializable(ContravariantMonoidal[Eq]))

    case class Something(a: Int, b: Int, c: Int)

    test("Eq returned by Eq.compose yields true on all underlying Eq's true") {
        val somethingEq = Eq.compose[Something](
            Eq.by(_.a), Eq.by(_.b), Eq.by(_.c)
        )

        val sut = Something(1, 2, 3)

        somethingEq.eqv(sut, sut) shouldBe true
    }

    test("Eq returned by Eq.compose yields false if some of underlying Eq's false") {
        val somethingEq = Eq.compose[Something](
            Eq.by(_.a), Eq.by(_.b), Eq.by(_.c)
        )

        val sut = Something(1, 2, 3)

        somethingEq.eqv(sut, sut.copy(a = 4)) shouldBe false
        somethingEq.eqv(sut, sut.copy(b = 4)) shouldBe false
        somethingEq.eqv(sut, sut.copy(c = 4)) shouldBe false
    }
}
