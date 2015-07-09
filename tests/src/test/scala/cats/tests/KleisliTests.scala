package cats
package tests

import cats.arrow.{Split, Arrow}
import cats.data.Kleisli
import cats.functor.Strong
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

class KleisliTests extends CatsSuite {
  implicit def kleisliEq[F[_], A, B](implicit A: Arbitrary[A], FB: Eq[F[B]]): Eq[Kleisli[F, A, B]] =
    Eq.by[Kleisli[F, A, B], A => F[B]](_.run)

  {
    implicit val kleisliArrow = Kleisli.kleisliArrow[Option]
    checkAll("Kleisli[Option, Int, Int]", ArrowTests[Kleisli[Option, ?, ?]].arrow[Int, Int, Int, Int, Int, Int])
    checkAll("Arrow[Kleisli[Option, ?, ?]]", SerializableTests.serializable(Arrow[Kleisli[Option, ?, ?]]))
  }

  {
    implicit val kleisliMonad = Kleisli.kleisliMonad[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", MonadTests[Kleisli[Option, Int, ?]].monad[Int, Int, Int])
    checkAll("Monad[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Monad[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val kleisliSplit = Kleisli.kleisliSplit[Option]
    checkAll("Kleisli[Option, Int, Int]", SplitTests[Kleisli[Option, ?, ?]].split[Int, Int, Int, Int, Int, Int])
    checkAll("Split[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Split[Kleisli[Option, ?, ?]]))
  }

  {
    implicit val kleisliStrong = Kleisli.kleisliStrong[Option]
    checkAll("Kleisli[Option, Int, Int]", StrongTests[Kleisli[Option, ?, ?]].strong[Int, Int, Int, Int, Int, Int])
    checkAll("Strong[Kleisli[Option, ?, ?]]", SerializableTests.serializable(Strong[Kleisli[Option, ?, ?]]))
  }

  {
    implicit val kleisliFlatMap = Kleisli.kleisliFlatMap[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", FlatMapTests[Kleisli[Option, Int, ?]].flatMap[Int, Int, Int])
    checkAll("FlatMap[Kleisli[Option, Int, ?]]", SerializableTests.serializable(FlatMap[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val kleisliApplicative = Kleisli.kleisliApplicative[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", ApplicativeTests[Kleisli[Option, Int, ?]].applicative[Int, Int, Int])
    checkAll("Applicative[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Applicative[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val kleisliApply = Kleisli.kleisliApply[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", ApplyTests[Kleisli[Option, Int, ?]].apply[Int, Int, Int])
    checkAll("Apply[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Apply[Kleisli[Option, Int, ?]]))
  }

  {
    implicit val kleisliFunctor = Kleisli.kleisliFunctor[Option, Int]
    checkAll("Kleisli[Option, Int, Int]", FunctorTests[Kleisli[Option, Int, ?]].functor[Int, Int, Int])
    checkAll("Functor[Kleisli[Option, Int, ?]]", SerializableTests.serializable(Functor[Kleisli[Option, Int, ?]]))
  }

  check {
    forAll { (f: Int => Option[String], g: Int => Int, i: Int) =>
      f(g(i)) == Kleisli.local[Option, String, Int](g)(Kleisli.function(f)).run(i)
    }
  }

  check {
    forAll { (i: Int) =>
      Kleisli.pure[Option, Int, Int](i).run(i) == Kleisli.ask[Option, Int].run(i)
    }
  }

  test("lift") {
    val f = Kleisli.function { (x: Int) => (Some(x + 1): Option[Int]) }
    val l = f.lift[List]
    assert((List(1, 2, 3) >>= l.run) == List(Some(2), Some(3), Some(4)))
  }

  test("transform") {
    val opt = Kleisli.function { (x: Int) => Option(x.toDouble) }
    val optToList = new (Option ~> List) { def apply[A](fa: Option[A]): List[A] = fa.toList }
    val list = opt.transform(optToList)

    val is = 0.to(10).toList
    assert(is.map(list.run) == is.map(Kleisli.function { (x: Int) => List(x.toDouble) }.run))
  }
}
