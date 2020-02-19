package cats.free

import cats.{Id, InvariantMonoidal}
import cats.arrow.FunctionK
import cats.kernel.Eq
import cats.instances.all._
import cats.laws.discipline.{InvariantMonoidalTests, MiniInt, SerializableTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.all._
import cats.tests.BinCodecInvariantMonoidalSuite._
import cats.tests.CatsSuite
import org.scalacheck.{Arbitrary, Gen}

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

  implicit val isoFreeBinCodec: Isomorphisms[FreeInvariantMonoidal[BinCodec, *]] =
    Isomorphisms.invariant[FreeInvariantMonoidal[BinCodec, *]]

  checkAll("FreeInvariantMonoidal[BinCodec, *]",
           InvariantMonoidalTests[FreeInvariantMonoidal[BinCodec, *]].invariantMonoidal[MiniInt, Boolean, Boolean])
  checkAll("InvariantMonoidal[FreeInvariantMonoidal[BinCodec, *]]",
           SerializableTests.serializable(InvariantMonoidal[FreeInvariantMonoidal[BinCodec, *]]))

  test("FreeInvariantMonoidal#fold") {
    forAll { (i1: BinCodec[MiniInt]) =>
      val n = MiniInt.unsafeFromInt(2)
      val i2 = InvariantMonoidal[BinCodec].point(n)
      val iExpr = i1.product(i2.imap(_ * n)(_ / n))

      val f1 = FreeInvariantMonoidal.lift[BinCodec, MiniInt](i1)
      val f2 = FreeInvariantMonoidal.pure[BinCodec, MiniInt](n)
      val fExpr = f1.product(f2.imap(_ * n)(_ / n))

      fExpr.fold should ===(iExpr)
    }
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
