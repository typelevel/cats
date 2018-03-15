package cats
package free

import cats.arrow.FunctionK
import cats.tests.CatsSuite
import cats.laws.discipline.{ ContravariantTests, SerializableTests }

import org.scalacheck.{ Arbitrary }

class ContravariantCoyonedaSuite extends CatsSuite {

  // If we can generate functions we can generate an interesting ContravariantCoyoneda.
  implicit def contravariantCoyonedaArbitrary[F[_], A, T](
    implicit F: Arbitrary[A => T]
  ): Arbitrary[ContravariantCoyoneda[? => T, A]] =
    Arbitrary(F.arbitrary.map(ContravariantCoyoneda.lift[? => T, A](_)))

  // We can't really test that functions are equal but we can try it with a bunch of test data.
  implicit def contravariantCoyonedaEq[A: Arbitrary, T](
    implicit eqft: Eq[T]): Eq[ContravariantCoyoneda[? => T, A]] =
    new Eq[ContravariantCoyoneda[? => T, A]] {
      def eqv(cca: ContravariantCoyoneda[? => T, A], ccb: ContravariantCoyoneda[? => T, A]): Boolean =
        Arbitrary.arbitrary[List[A]].sample.get.forall { a =>
          eqft.eqv(cca.run.apply(a), ccb.run.apply(a))
        }
    }

  // This instance cannot be summoned implicitly. This is not specific to contravariant coyoneda;
  // it doesn't work for Functor[Coyoneda[? => String, ?]] either.
  implicit val contravariantContravariantCoyonedaToString: Contravariant[ContravariantCoyoneda[? => String, ?]] =
    ContravariantCoyoneda.catsFreeContravariantFunctorForContravariantCoyoneda[? => String]

  checkAll("ContravariantCoyoneda[? => String, Int]", ContravariantTests[ContravariantCoyoneda[? => String, ?]].contravariant[Int, Int, Int])
  checkAll("Contravariant[ContravariantCoyoneda[Option, ?]]", SerializableTests.serializable(Contravariant[ContravariantCoyoneda[Option, ?]]))

  test("mapK and run is same as applying natural trans") {
    forAll { (b: Boolean) =>
      val nt = λ[(? => String) ~> (? => Int)](f => s => f(s).length)
      val o = (b: Boolean) => b.toString
      val c = ContravariantCoyoneda.lift[? => String, Boolean](o)
      c.mapK[? => Int](nt).run.apply(b) === nt(o).apply(b)
    }
  }

  test("contramap order") {
    ContravariantCoyoneda
      .lift[? => Int, String](_.count(_ == 'x'))
      .contramap((s: String) => s + "x")
      .contramap((s: String) => s * 3)
      .run.apply("foo") === 3
  }

  test("stack-safe contramapmap") {
    def loop(n: Int, acc: ContravariantCoyoneda[? => Int, Int]): ContravariantCoyoneda[? => Int, Int] =
      if (n <= 0) acc
      else loop(n - 1, acc.contramap((_: Int) + 1))
    loop(20000, ContravariantCoyoneda.lift[? => Int, Int](a => a)).run.apply(10)
  }

  test("run, foldMap consistent") {
    forAll { (
      c: ContravariantCoyoneda[? => Int, String],
      f: Byte => String,
      g: Float => Byte,
      s: Float
    ) =>
      val cʹ = c.contramap(f).contramap(g) // just to ensure there's some structure
      val h  = cʹ.foldMap[? => Int](FunctionK.id[? => Int])
      cʹ.run.apply(s) === h(s)
    }
  }

}
