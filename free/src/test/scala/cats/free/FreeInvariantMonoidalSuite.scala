package cats
package tests

import cats.arrow.FunctionK
import cats.free.FreeInvariantMonoidal
import cats.laws.discipline.{InvariantMonoidalTests, SerializableTests}
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import org.scalacheck.{Arbitrary, Gen}
import cats.tests.CsvCodecInvariantMonoidalSuite._

class FreeInvariantMonoidalSuite extends CatsSuite {
  implicit def freeInvariantMonoidalArbitrary[F[_], A](implicit F: Arbitrary[F[A]],
                                                       A: Arbitrary[A]): Arbitrary[FreeInvariantMonoidal[F, A]] =
    Arbitrary(
      Gen.oneOf(A.arbitrary.map(FreeInvariantMonoidal.pure[F, A]), F.arbitrary.map(FreeInvariantMonoidal.lift[F, A]))
    )

  implicit def freeInvariantMonoidalEq[S[_]: InvariantMonoidal, A](
    implicit SA: Eq[S[A]]
  ): Eq[FreeInvariantMonoidal[S, A]] =
    new Eq[FreeInvariantMonoidal[S, A]] {
      def eqv(a: FreeInvariantMonoidal[S, A], b: FreeInvariantMonoidal[S, A]): Boolean = {
        val nt = FunctionK.id[S]
        SA.eqv(a.foldMap(nt), b.foldMap(nt))
      }
    }

  implicit val isoFreeCsvCodec = Isomorphisms.invariant[FreeInvariantMonoidal[CsvCodec, ?]]

  checkAll("FreeInvariantMonoidal[CsvCodec, ?]",
           InvariantMonoidalTests[FreeInvariantMonoidal[CsvCodec, ?]].invariantMonoidal[Int, Int, Int])
  checkAll("InvariantMonoidal[FreeInvariantMonoidal[CsvCodec, ?]]",
           SerializableTests.serializable(InvariantMonoidal[FreeInvariantMonoidal[CsvCodec, ?]]))

  test("FreeInvariantMonoidal#fold") {
    val n = 2
    val i1 = numericSystemCodec(8)
    val i2 = InvariantMonoidal[CsvCodec].point(n)
    val iExpr = i1.product(i2.imap(_ * 2)(_ / 2))

    val f1 = FreeInvariantMonoidal.lift[CsvCodec, Int](i1)
    val f2 = FreeInvariantMonoidal.pure[CsvCodec, Int](n)
    val fExpr = f1.product(f2.imap(_ * 2)(_ / 2))

    fExpr.fold should ===(iExpr)
  }

  implicit val idIsInvariantMonoidal: InvariantMonoidal[Id] = new InvariantMonoidal[Id] {
    def product[A, B](fa: Id[A], fb: Id[B]): Id[(A, B)] = fa -> fb
    def imap[A, B](fa: Id[A])(f: A => B)(g: B => A): Id[B] = f(fa)
    def unit: Id[Unit] = ()
  }

  test("FreeInvariantMonoidal#compile") {
    val x = FreeInvariantMonoidal.lift[Id, Int](1)
    val y = FreeInvariantMonoidal.pure[Id, Int](2)
    val p = x.imap(_ * 2)(_ / 2)
    val nt = FunctionK.id[Id]
    val r1 = y.product(p)
    val r2 = r1.compile(nt)
    r1.foldMap(nt) should ===(r2.foldMap(nt))
  }

  test("FreeInvariantMonoidal#analyze") {
    type G[A] = List[Int]
    val countingNT = λ[FunctionK[List, G]](la => List(la.length))

    val fli1 = FreeInvariantMonoidal.lift[List, Int](List(1, 3, 5, 7))
    fli1.analyze[G[Int]](countingNT) should ===(List(4))

    val fli2 = FreeInvariantMonoidal.lift[List, Int](List.empty)
    fli2.analyze[G[Int]](countingNT) should ===(List(0))
  }
}
