package cats
package free

import cats.arrow.FunctionK
import cats.data.{EitherK, Validated}
import cats.laws.discipline.{ParallelTests, SemigroupalTests, FoldableTests, MonadTests, SerializableTests, TraverseTests}
import cats.laws.discipline.arbitrary._
import cats.tests.CatsSuite

import org.scalacheck.{Arbitrary, Gen, Cogen}
import Arbitrary.arbFunction1

class FreeSuite extends CatsSuite {
  import FreeSuite._
  import FreeApplicativeSuite._

  implicit val iso = SemigroupalTests.Isomorphisms.invariant[Free[Option, ?]]

  checkAll("Free[Option, ?]", MonadTests[Free[Option, ?]].monad[Int, Int, Int])
  checkAll("Monad[Free[Option, ?]]", SerializableTests.serializable(Monad[Free[Option, ?]]))

  checkAll("Free[Either[String, ?], ?], FreeApplicative[Validated[String, ?], ?]", ParallelTests[Free[Either[String, ?], ?], FreeApplicative[Validated[String, ?], ?]].parallel[Int, String])
  checkAll("Parallel[Free[Either[String, ?], ?], FreeApplicative[Validated[String, ?], ?]]", SerializableTests.serializable(Parallel[Free[Either[String, ?], ?], FreeApplicative[Validated[String, ?], ?]]))

  locally {
    implicit val instance = Free.catsFreeFoldableForFree[Option]

    checkAll("Free[Option, ?]", FoldableTests[Free[Option,?]].foldable[Int,Int])
    checkAll("Foldable[Free[Option,?]]", SerializableTests.serializable(Foldable[Free[Option,?]]))
  }

  locally {
    implicit val instance = Free.catsFreeTraverseForFree[Option]
    checkAll("Free[Option,?]", TraverseTests[Free[Option,?]].traverse[Int, Int, Int, Int, Option, Option])
    checkAll("Traverse[Free[Option,?]]", SerializableTests.serializable(Traverse[Free[Option,?]]))
  }

  test("toString is stack-safe") {
    val r = Free.pure[List, Int](333)
    val rr = (1 to 1000000).foldLeft(r)((r, _) => r.map(_ + 1))
    rr.toString.length should be > 0
  }

  test("compile id"){
    forAll { x: Free[List, Int] =>
      x.compile(FunctionK.id[List]) should === (x)
      val fk = Free.compile(FunctionK.id[List])
      fk(x) === x
    }
  }

  test("defer doesn't change value"){
    forAll { x: Free[List, Int] =>
      Free.defer(x) should === (x)
    }
  }

  test("defer is lazy"){
    def yikes[F[_], A]: Free[F, A] = throw new RuntimeException("blargh")
    // this shouldn't throw an exception unless we try to run it
    val _ = Free.defer(yikes[Option, Int])
  }

  test("compile consistent with foldMap"){
    forAll { x: Free[List, Int] =>
      val mapped = x.compile(headOptionU)
      val folded = mapped.foldMap(FunctionK.id[Option])
      folded should === (x.foldMap(headOptionU))

      val fk = Free.foldMap(headOptionU)
      folded should === (fk(x))
    }
  }

  test("tailRecM is stack safe") {
    val n = 50000
    val fa = Monad[Free[Option, ?]].tailRecM(0)(i =>
      Free.pure[Option, Either[Int, Int]](if (i < n) Left(i+1) else Right(i)))
    fa should === (Free.pure[Option, Int](n))
  }

  test("foldMap is stack safe") {
    trait FTestApi[A]
    case class TB(i: Int) extends FTestApi[Int]

    type FTest[A] = Free[FTestApi, A]

    def tb(i: Int): FTest[Int] = Free.liftF(TB(i))

    def a(i: Int): FTest[Int] = for {
      j <- tb(i)
      z <- if (j<10000) a(j) else Free.pure[FTestApi, Int](j)
    } yield z

    def runner: FunctionK[FTestApi,Id] = λ[FunctionK[FTestApi,Id]] {
      case TB(i) => i+1
    }

    assert(10000 == a(0).foldMap(runner))
  }

  test(".runTailRec") {
    val r = Free.pure[List, Int](12358)
    def recurse(r: Free[List, Int], n: Int): Free[List, Int] =
      if (n > 0) recurse(r.flatMap(x => Free.pure(x + 1)), n - 1) else r
    val res = recurse(r, 100000).runTailRec
    assert(res == List(112358))
  }

  test(".run") {
    val r = Free.pure[Id, Int](12358)
    def recurse(r: Free[Id, Int], n: Int): Free[Id, Int] =
      if (n > 0) recurse(r.flatMap(x => Free.pure(x + 1)), n - 1) else r
    val res = recurse(r, 100000).run
    assert(res == 112358)
  }

  sealed trait Test1Algebra[A]

  case class Test1[A](value : Int, f: Int => A) extends Test1Algebra[A]

  def test1[A](value: Int, f: Int => A): Test1Algebra[A] = Test1(value, f)

  object Test1Algebra {
    implicit def test1AlgebraAFunctor: Functor[Test1Algebra] =
      new Functor[Test1Algebra] {
        def map[A, B](a: Test1Algebra[A])(f: A => B): Test1Algebra[B] = a match {
          case Test1(k, h) => Test1(k, x => f(h(x)))
        }
      }

    implicit def test1AlgebraArbitrary[A](implicit seqArb: Arbitrary[Int], intAArb : Arbitrary[Int => A]): Arbitrary[Test1Algebra[A]] =
      Arbitrary(for {s <- seqArb.arbitrary; f <- intAArb.arbitrary} yield Test1(s, f))
  }

  sealed trait Test2Algebra[A]

  case class Test2[A](value : Int, f: Int => A) extends Test2Algebra[A]

  def test2[A](value: Int, f: Int => A): Test2Algebra[A] = Test2(value, f)

  object Test2Algebra {
    implicit def test2AlgebraAFunctor: Functor[Test2Algebra] =
      new Functor[Test2Algebra] {
        def map[A, B](a: Test2Algebra[A])(f: A => B): Test2Algebra[B] = a match {
          case Test2(k, h) => Test2(k, x => f(h(x)))
        }
      }

    implicit def test2AlgebraArbitrary[A](implicit seqArb: Arbitrary[Int], intAArb : Arbitrary[Int => A]): Arbitrary[Test2Algebra[A]] =
      Arbitrary(for {s <- seqArb.arbitrary; f <- intAArb.arbitrary} yield Test2(s, f))
  }

  type T[A] = EitherK[Test1Algebra, Test2Algebra, A]

  object Test1Interpreter extends FunctionK[Test1Algebra,Id] {
    override def apply[A](fa: Test1Algebra[A]): Id[A] = fa match {
      case Test1(k, h) => h(k)
    }
  }

  object Test2Interpreter extends FunctionK[Test2Algebra,Id] {
    override def apply[A](fa: Test2Algebra[A]): Id[A] = fa match {
      case Test2(k, h) => h(k)
    }
  }

  val eitherKInterpreter: FunctionK[T,Id] = Test1Interpreter or Test2Interpreter

  test(".inject") {
    forAll { (x: Int, y: Int) =>
        def res[F[_]]
            (implicit I0: Test1Algebra :<: F,
            I1: Test2Algebra :<: F): Free[F, Int] = {
          for {
            a <- Free.inject[Test1Algebra, F](test1(x, identity))
            b <- Free.inject[Test2Algebra, F](test2(y, identity))
          } yield a + b
        }
      (res[T] foldMap eitherKInterpreter) == (x + y) should ===(true)
    }
  }

  test(".liftInject") {
    forAll { (x: Int, y: Int) =>
      def res[F[_]]
      (implicit I0: Test1Algebra :<: F,
       I1: Test2Algebra :<: F): Free[F, Int] = {
        for {
          a <- Free.liftInject[F](test1(x, identity))
          b <- Free.liftInject[F](test2(y, identity))
        } yield a + b
      }
      (res[T] foldMap eitherKInterpreter) == (x + y) should ===(true)
    }
  }

  val x: Free[T, Int] = Free.inject[Test1Algebra, T](Test1(1, identity))

  test(".injectRoll") {
    def distr[F[_], A](f: Free[F, A])
                      (implicit
                       F: Functor[F],
                       I0: Test1Algebra :<: F,
                       I1: Test2Algebra :<: F): Option[Free[F, A]] =
      for {
        Test1(x, h) <- Free.match_[F, Test1Algebra, A](f)
        Test2(y, k) <- Free.match_[F, Test2Algebra, A](h(x))
      } yield k(x + y)

    forAll { (x: Int, y: Int) =>
      val expr1: Free[T, Int] = Free.injectRoll[T, Test1Algebra, Int](Test1(x, Free.pure))
      val expr2: Free[T, Int] = Free.injectRoll[T, Test2Algebra, Int](Test2(y, Free.pure))
      val res = distr[T, Int](expr1 *> expr2)
      res.map(_.foldMap(eitherKInterpreter)) should === (Some(Free.pure[Id, Int](x + y).foldMap(FunctionK.id)))
    }
  }
}

object FreeSuite extends FreeSuiteInstances {
  import cats.instances.function._

  implicit def trampolineArbitrary[A:Arbitrary]: Arbitrary[Trampoline[A]] =
    freeArbitrary[Function0, A]

  implicit def trampolineEq[A:Eq]: Eq[Trampoline[A]] =
    freeEq[Function0, A]
}

sealed trait FreeSuiteInstances {
  val headOptionU = λ[FunctionK[List,Option]](_.headOption)

  private def freeGen[F[_], A](maxDepth: Int)(implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Gen[Free[F, A]] = {
    val noFlatMapped = Gen.oneOf(
      A.arbitrary.map(Free.pure[F, A]),
      F.arbitrary.map(Free.liftF[F, A]))

    val nextDepth = Gen.chooseNum(1, math.max(1, maxDepth - 1))

    def withFlatMapped = for {
      fDepth <- nextDepth
      freeDepth <- nextDepth
      f <- arbFunction1[A, Free[F, A]](Arbitrary(freeGen[F, A](fDepth)), Cogen[Unit].contramap(_ => ())).arbitrary
      freeFA <- freeGen[F, A](freeDepth)
    } yield freeFA.flatMap(f)

    if (maxDepth <= 1) noFlatMapped
    else Gen.oneOf(noFlatMapped, withFlatMapped)
  }

  implicit def freeArbitrary[F[_], A](implicit F: Arbitrary[F[A]], A: Arbitrary[A]): Arbitrary[Free[F, A]] =
    Arbitrary(freeGen[F, A](4))

  implicit def freeEq[S[_]: Monad, A](implicit SA: Eq[S[A]]): Eq[Free[S, A]] =
    new Eq[Free[S, A]] {
      def eqv(a: Free[S, A], b: Free[S, A]): Boolean =
        SA.eqv(a.runM(identity),  b.runM(identity))
    }
}
