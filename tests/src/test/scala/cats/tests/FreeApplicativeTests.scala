package cats
package tests

import cats.arrow.NaturalTransformation
import cats.free.FreeApplicative
import cats.laws.discipline.{MonoidalTests, ApplicativeTests, SerializableTests}
import cats.laws.discipline.eq.{tuple3Eq, tuple2Eq}
import cats.data.Const
import cats.state.State

import org.scalacheck.{Arbitrary, Gen}

class FreeApplicativeTests extends CatsSuite {
  implicit def freeApplicativeArbitrary[F[_], A](implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Arbitrary[FreeApplicative[F, A]] =
    Arbitrary(
      Gen.oneOf(
        A.arbitrary.map(FreeApplicative.pure[F, A]),
        F.arbitrary.map(FreeApplicative.lift[F, A])))

  implicit def freeApplicativeEq[S[_]: Applicative, A](implicit SA: Eq[S[A]]): Eq[FreeApplicative[S, A]] =
    new Eq[FreeApplicative[S, A]] {
      def eqv(a: FreeApplicative[S, A], b: FreeApplicative[S, A]): Boolean = {
        val nt = NaturalTransformation.id[S]
        SA.eqv(a.foldMap(nt), b.foldMap(nt))
      }
    }

  implicit val iso = MonoidalTests.Isomorphisms.invariant[FreeApplicative[Option, ?]]

  checkAll("FreeApplicative[Option, ?]", ApplicativeTests[FreeApplicative[Option, ?]].applicative[Int, Int, Int])
  checkAll("Monad[FreeApplicative[Option, ?]]", SerializableTests.serializable(Applicative[FreeApplicative[Option, ?]]))

  test("FreeApplicative#fold") {
    val n = 2
    val o1 = Option(1)
    val o2 = Applicative[Option].pure(n)

    val x = FreeApplicative.lift[Option, Int](o1)
    val y = FreeApplicative.pure[Option, Int](n)
    val f = x.map(i => (j: Int) => i + j)
    val r = y.ap(f)
    r.fold should === (Apply[Option].map2(o1, o2)(_ + _))
  }

  test("FreeApplicative#compile") {
    val x = FreeApplicative.lift[Id, Int](1)
    val y = FreeApplicative.pure[Id, Int](2)
    val f = x.map(i => (j: Int) => i + j)
    val nt = NaturalTransformation.id[Id]
    val r1 = y.ap(f)
    val r2 = r1.compile(nt)
    r1.foldMap(nt) should === (r2.foldMap(nt))
  }

  test("FreeApplicative#monad") {
    val x = FreeApplicative.lift[Id, Int](1)
    val y = FreeApplicative.pure[Id, Int](2)
    val f = x.map(i => (j: Int) => i + j)
    val r1 = y.ap(f)
    val r2 = r1.monad
    val nt =
      new NaturalTransformation[Id, Id] {
        def apply[A](fa: Id[A]): Id[A] = fa
      }
    r1.foldMap(nt) should === (r2.foldMap(nt))
  }

  // Ensure that syntax and implicit resolution work as expected.
  // If it compiles, it passes the "test".
  object SyntaxTests {

    // fixed by #568
    val fli1 = FreeApplicative.lift[List, Int](List(1, 3, 5, 7))
    val fli2 = FreeApplicative.lift[List, Int](List(1, 3, 5, 7))
    (fli1 |@| fli2).map(_ + _)
  }

  test("FreeApplicative#analyze") {
    type G[A] = List[Int]
    val countingNT = new NaturalTransformation[List, G] {
      def apply[A](la: List[A]): G[A] = List(la.length)
    }

    val fli1 = FreeApplicative.lift[List, Int](List(1, 3, 5, 7))
    fli1.analyze[G[Int]](countingNT) should === (List(4))

    val fli2 = FreeApplicative.lift[List, Int](List.empty)
    fli2.analyze[G[Int]](countingNT) should ===(List(0))
  }

  test("foldMap order of effects - regression check for #799") {
    trait Foo[A] {
      def getA: A
    }
    final case class Bar(getA: Int) extends Foo[Int]
    final case class Baz(getA: Long) extends Foo[Long]

    type Dsl[A] = FreeApplicative[Foo, A]

    type Tracked[A] = State[String, A]

    val f: Foo ~> Tracked = new (Foo ~> Tracked) {
      def apply[A](fa: Foo[A]): Tracked[A] = State[String, A]{ s0 =>
        (s0 + fa.toString + ";", fa.getA)
      }
    }

    val x: Dsl[Int] = FreeApplicative.lift(Bar(3))
    val y: Dsl[Long] = FreeApplicative.lift(Baz(5L))

    val z1: Dsl[Long] = Apply[Dsl].map2(x, y)((x, y) => x.toLong + y)
    val z2: Dsl[Long] = Apply[Dsl].map2(y, x)((y, x) => x.toLong + y)

    z1.foldMap(f).run("").value should === (("Bar(3);Baz(5);", 8L))
    z2.foldMap(f).run("").value should === (("Baz(5);Bar(3);", 8L))
  }

  test("analyze order of effects - regression check for #799") {
    type Dsl[A] = FreeApplicative[Id, A]
    val x: Dsl[String] = FreeApplicative.lift[Id, String]("x")
    val y: Dsl[String] = FreeApplicative.lift[Id, String]("y")

    val z = Apply[Dsl].map2(x, y)((_, _) => ())

    val asString: Id ~> λ[α => String] = new (Id ~> λ[α => String]) {
      def apply[A](a: A): String = a.toString
    }

    z.analyze(asString) should === ("xy")
  }
}
