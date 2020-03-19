package cats.tests

import cats.{Comonad, Foldable, Id, Traverse}
import cats.data.{Env, EnvT}
import cats.instances.all._
import cats.kernel.Eq
import cats.laws.discipline.{ComonadTests, FoldableTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Cogen}

class EnvTSuite extends CatsSuite {

  {
    implicit val idComonad: Comonad[Env[Int, *]] = EnvT.catsDataEnvTComonad[Id, Int]
    implicit val arbEnv: Arbitrary[Env[Int, Int]] = catsLawsArbitraryForEnvT[Id, Int, Int]
    implicit val cogenEnv: Cogen[Env[Int, Int]] = catsLawsCogenForEnvT[Id, Int, Int]
    implicit val eqEnv: Eq[Env[Int, Int]] = cats.laws.discipline.eq.catsLawsEqForEnvT[Id, Int, Int]
    implicit val eqEnvEnv: Eq[Env[Int, Env[Int, Int]]] =
      cats.laws.discipline.eq.catsLawsEqForEnvT[Id, Int, EnvT[Id, Int, Int]]
    implicit val eqEnvEnvEnv: Eq[Env[Int, Env[Int, Env[Int, Int]]]] =
      cats.laws.discipline.eq.catsLawsEqForEnvT[Id, Int, Env[Int, Env[Int, Int]]]

    checkAll("Comonad[Env[Int, *]]", ComonadTests[Env[Int, *]].comonad[Int, Int, Int])
    checkAll("Comonad[Env[Int, *]]", SerializableTests.serializable(Comonad[Env[Int, *]]))
  }

  locally {
    implicit val instance: Foldable[Env[Int, *]] = EnvT.catsDataEnvTFoldable[Id, Int]
    implicit val arbEnv: Arbitrary[Env[Int, Int]] = catsLawsArbitraryForEnvT[Id, Int, Int]
    implicit val eqEnv: Eq[Env[Int, Int]] = cats.laws.discipline.eq.catsLawsEqForEnvT[Id, Int, Int]
    checkAll("Env[Int, *]", FoldableTests[Env[Int, *]].foldable[Int, Int])
    checkAll("Foldable[Env[Int, *]]", SerializableTests.serializable(Foldable[Env[Int, *]]))
  }

  locally {
    implicit val instance: Traverse[Env[Int, *]] = EnvT.catsDataEnvTTraverse[Id, Int]
    implicit val arbEnv: Arbitrary[Env[Int, Int]] = catsLawsArbitraryForEnvT[Id, Int, Int]
    implicit val eqEnv: Eq[Env[Int, Int]] = cats.laws.discipline.eq.catsLawsEqForEnvT[Id, Int, Int]
    checkAll("Env[Int, *]", TraverseTests[Env[Int, *]].traverse[Int, Int, Int, Int, Id, Id])
    checkAll("Traverse[Env[Int, *]]", SerializableTests.serializable(Traverse[Env[Int, *]]))
  }

  test("ask and asks are consistent") {
    forAll { (env: Env[String, String]) =>
      env.asks(identity) should ===(env.ask)
    }
  }

  test("local and asks are consistent") {
    forAll { (env: Env[String, String], f: String => Int) =>
      env.asks(f) should ===(env.local(f).ask)
    }
  }
}
