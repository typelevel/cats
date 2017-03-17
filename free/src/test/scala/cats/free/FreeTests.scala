package cats
package free

import cats.tests.CatsSuite
import cats.arrow.FunctionK
import cats.laws.discipline.{CartesianTests, MonadTests, SerializableTests}
import cats.laws.discipline.arbitrary.catsLawsArbitraryForFn0
import org.scalacheck.{Arbitrary, Cogen, Gen}
import Arbitrary.arbFunction1
import cats.data.EitherT

class FreeTests extends CatsSuite {
  import FreeTests._

  implicit val iso = CartesianTests.Isomorphisms.invariant[Free[Option, ?]]

  checkAll("Free[Option, ?]", MonadTests[Free[Option, ?]].monad[Int, Int, Int])
  checkAll("Monad[Free[Option, ?]]", SerializableTests.serializable(Monad[Free[Option, ?]]))

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

  test("suspend doesn't change value"){
    forAll { x: Free[List, Int] =>
      Free.suspend(x) should === (x)
    }
  }

  test("suspend is lazy"){
    def yikes[F[_], A]: Free[F, A] = throw new RuntimeException("blargh")
    // this shouldn't throw an exception unless we try to run it
    val _ = Free.suspend(yikes[Option, Int])
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

  test(".foldLeftM") {
    // you can see .foldLeftM traversing the entire structure by
    // changing the constant argument to .take and observing the time
    // this test takes.
    val ns = Stream.from(1).take(1000)
    val res = Free.foldLeftM[Stream, Either[Int, ?], Int, Int](ns, 0) { (sum, n) =>
      if (sum >= 2) Either.left(sum) else Either.right(sum + n)
    }
    assert(res == Either.left(3))
  }

  test(".foldLeftM short-circuiting") {
    val ns = Stream.continually(1)
    val res = Free.foldLeftM[Stream, Either[Int, ?], Int, Int](ns, 0) { (sum, n) =>
      if (sum >= 100000) Either.left(sum) else Either.right(sum + n)
    }
    assert(res == Either.left(100000))
  }

  locally { // cata/cataM

    test(".cata") {
      val r: Free[Option, Int] = Free.roll(Some(Free.roll(Some(Free.pure(1)))))
      val non: Free[Option, Int] = Free.roll[Option, Int](None)
      val f: Either[Option[String], Int] => Eval[String] = {
        case Right(b) => Eval.now(b.toString)
        case Left(fa) => Eval.now(fa.toString)
      }
      val resr: String = Free.cata[Option, Int, String](r)(f).value
      val resnon: String = Free.cata[Option, Int, String](non)(f).value
      assert(resr == "Some(Some(1))")
      assert(resnon == "None")
    }

    def rollSome(size: Int, acc: Free[Option, Int]): Free[Option, Int] =
      if (size == 0) acc else rollSome(size - 1, Free.roll(Some(acc)))

    val threeThousandSomeOne: Free[Option, Int] = rollSome(3000, Free.pure(1))
    val threeThousandPlussesOne: String = String.valueOf(Array.fill(3000)('+')) + "1"

    test(".cata stack-safety") {

      val plusForSomeOrZeroStrForNone: Either[Option[String], Int] => Eval[String] = {
        case Right(b) => Eval.now(b.toString)
        case Left(fa) => Eval.now(fa.fold("0")("+" + _))
      }

      assert(Free.cata[Option, Int, String](threeThousandSomeOne)(plusForSomeOrZeroStrForNone).value == threeThousandPlussesOne)
    }

    type EitherEvalInt[A] = EitherT[Eval, Int, A]

    val plusForSomeOrZeroForNone: Either[Option[String], Int] => EitherEvalInt[String] = {
      case Right(b) => EitherT.fromEither[Eval].apply(Right(b.toString))
      case Left(fa) => EitherT.fromEither[Eval].apply(fa.fold[Either[Int, String]](Left(0))(s => Right("+" + s)))
    }
    val inclusion = new (Eval ~> EitherEvalInt) {
      override def apply[A](fa: Eval[A]): EitherEvalInt[A] = EitherT.right(fa)
    }

    test(".cataM") {
      val r: Free[Option, Int] = Free.roll(Some(Free.roll(Some(Free.pure(1)))))
      val non: Free[Option, Int] = Free.roll[Option, Int](None)
      val resr = Free.cataM[Option, EitherEvalInt, Int, String](r)(plusForSomeOrZeroForNone)(inclusion).value.value
      val resnon = Free.cataM[Option, EitherEvalInt, Int, String](non)(plusForSomeOrZeroForNone)(inclusion).value.value
      assert(resr == Right("++1"))
      assert(resnon == Left(0))
    }

    test(".cataM stack-safety") {
      val resr =
        Free.cataM[Option, EitherEvalInt, Int, String](threeThousandSomeOne)(plusForSomeOrZeroForNone)(inclusion)
          .value.value
      assert(resr == Right(threeThousandPlussesOne))
    }

  }

}

object FreeTests extends FreeTestsInstances {
  import cats.instances.function._

  implicit def trampolineArbitrary[A:Arbitrary]: Arbitrary[Trampoline[A]] =
    freeArbitrary[Function0, A]

  implicit def trampolineEq[A:Eq]: Eq[Trampoline[A]] =
    freeEq[Function0, A]
}

sealed trait FreeTestsInstances {
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
