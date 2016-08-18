package cats
package free

import cats._
import cats.arrow.FunctionK
import cats.data._
import cats.laws.discipline._
import cats.tests.{CatsSuite, ListWrapper}
import cats.instances.option._

import org.scalacheck.{Arbitrary, Gen}

class FreeTTests extends CatsSuite {

  import ListWrapper._
  import FreeTTests._
/*    
    checkAll("FreeT[ListWrapper, ListWrapper, Int]", FlatMapTests[FreeTListWrapper].flatMap[Int, Int, Int])
    checkAll("FlatMap[FreeT[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(FlatMap[FreeTListWrapper]))

    checkAll("FreeT[ListWrapper, ListWrapper, Int]", MonadTests[FreeTListWrapper].monad[Int, Int, Int])
    checkAll("Monad[FreeT[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(Monad[FreeTListWrapper]))
 
    checkAll("FreeT[ListWrapper, ListWrapper, Int]", SemigroupKTests[FreeTListWrapper].semigroupK[Int])
    checkAll("SemigroupK[FreeT[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(SemigroupK[FreeTListWrapper]))
  
    checkAll("FreeT[ListWrapper, ListWrapper, Int]", MonadCombineTests[FreeTListWrapper].monadCombine[Int, Int, Int])
    checkAll("MonadCombine[FreeT[ListWrapper, ListWrapper, ?]]", SerializableTests.serializable(MonadCombine[FreeTListWrapper]))
 */
  {
    implicit val eqXortTFA: Eq[XorT[FreeTListOption, Unit, Int]] = XorT.catsDataEqForXorT[FreeTListOption, Unit, Int]
    checkAll("FreeT[ListWrapper, Option, Int]", MonadErrorTests[FreeTListOption, Unit].monadError[Int, Int, Int])
    checkAll("MonadError[FreeT[ListWrapper, Option, ?], Unit]", SerializableTests.serializable(MonadError[FreeTListOption, Unit]))
  }

  {
    import StateT._
    checkAll("FreeT[ListWrapper, State[Int, ?], Int]", MonadStateTests[FreeTListState, Int].monadState[Int, Int, Int])
    checkAll("MonadState[FreeT[ListWrapper,State[Int, ?], ?], Int]", SerializableTests.serializable(MonadState[FreeTListState, Int]))
    }
  
   
  
  test("FlatMap stack safety tested with 50k flatMaps") {
    val expected = Applicative[FreeTListOption].pure(())
    val result =
      Monad[FreeTListOption].tailRecM(0)((i: Int) =>
        if (i < 50000)
          Applicative[FreeTListOption].pure(Xor.left[Int, Unit](i + 1))
        else
          Applicative[FreeTListOption].pure(Xor.right[Int, Unit](())))

    Eq[FreeTListOption[Unit]].eqv(expected, result) should ===(true)
  }

  test("Stack safe with 50k left-associated flatMaps") {
    val expected = Applicative[FreeTListOption].pure(())
    val result =
      (0 until 50000).foldLeft(Applicative[FreeTListOption].pure(()))(
        (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].pure(u))
      )

    Eq[FreeTListOption[Unit]].eqv(expected, result) should ===(true)
  }

  test("Stack safe with flatMap followed by 50k maps") {
    val expected = Applicative[FreeTListOption].pure(())
    val result =
      (0 until 50000).foldLeft(().pure[FreeTListOption].flatMap(_.pure[FreeTListOption]))(
        (fu, i) => fu.map(identity)
      )

    Eq[FreeTListOption[Unit]].eqv(expected, result) should ===(true)
  }

  test("hoist to universal id equivalent to original instance") {
    forAll { a: FreeTListOption[Int] =>
      val b = a.hoist(FunctionK.id)
      Eq[FreeTListOption[Int]].eqv(a, b) should ===(true)
    }
  }

  test("hoist stack-safety") {
    val a = (0 until 50000).foldLeft(Applicative[FreeTListOption].pure(()))(
      (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].pure(u))
    )
    val b = a.hoist(FunctionK.id)
  }

  test("transLift for FreeT requires only Functor") {
    implicit val transLiftInstance = FreeT.catsFreeTransLiftForFreeT[JustFunctor]
    val d: FreeT[JustFunctor, JustFunctor, Int] = transLiftInstance.liftT[JustFunctor, Int](JustFunctor(1))
  }

  test("interpret to universal id equivalent to original instance") {
    forAll { a: FreeTListOption[Int] =>
      val b = a.interpret(FunctionK.id)
      Eq[FreeTListOption[Int]].eqv(a, b) should ===(true)
    }
  }

  test("interpret stack-safety") {
    val a = (0 until 50000).foldLeft(Applicative[FreeTListOption].pure(()))(
      (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].pure(u))
    )
    val b = a.interpret(FunctionK.id) // used to overflow
  }

  test("foldMap consistent with runM") {
    implicit val listWrapperFunctor = ListWrapper.monad
    forAll { a: FreeTListOption[Int] =>
      val x = a.runM(_.list.headOption)
      val y = a.foldMap(headOption)
      Eq[Option[Int]].eqv(x, y) should ===(true)
    }
  }

  test("== should not return true for unequal instances") {
    val a = FreeT.pure[List, Option, Int](1).flatMap(x => FreeT.pure(2))
    val b = FreeT.pure[List, Option, Int](3).flatMap(x => FreeT.pure(4))
    a == b should be(false)
  }

  private[free] def liftTUCompilationTests() = {
    val a: String Xor Int = Xor.right(42)
    val b: FreeT[Option, String Xor ?, Int] = FreeT.liftTU(a)
  }
}

object FreeTTests extends FreeTTestsInstances

sealed trait FreeTTestsInstances {

  import FreeT._
  import Arbitrary._
  import org.scalacheck.Arbitrary
  import cats.kernel.instances.option._
  import cats.tests.StateTTests._
  import CartesianTests._

  type IntState[A] = State[Int, A]
  type FreeTListW[M[_], A] = FreeT[ListWrapper, M, A]
  type FreeTListWrapper[A] = FreeTListW[ListWrapper, A]
  type FreeTListOption[A] = FreeTListW[Option, A]
  type FreeTListState[A] = FreeT[IntState, IntState, A]
  type MonadRec[F[_]] = Monad[F] with RecursiveTailRecM[F]

  case class JustFunctor[A](a: A)

  implicit val listSemigroupK: SemigroupK[ListWrapper] = ListWrapper.semigroupK
  
  implicit val listWrapperMonad: MonadRec[ListWrapper] with Alternative[ListWrapper] = ListWrapper.monadCombine

  implicit val intStateMonad: MonadRec[IntState] = throw("can't summon this instance!")

  implicit val ftlWIso: Isomorphisms[FreeTListWrapper] = CartesianTests.Isomorphisms.invariant[FreeTListWrapper]

  implicit val ftlOIso: Isomorphisms[FreeTListOption] = CartesianTests.Isomorphisms.invariant[FreeTListOption]

  implicit val ftlSIso: Isomorphisms[FreeTListState] = CartesianTests.Isomorphisms.invariant[FreeTListState]
    
  implicit val jfFunctor: Functor[JustFunctor] = new Functor[JustFunctor] {
    override def map[A, B](fa: JustFunctor[A])(f: A => B): JustFunctor[B] = JustFunctor(f(fa.a))
  }

  object headOption extends (ListWrapper ~> Option) {
    def apply[A](l: ListWrapper[A]): Option[A] = l.list.headOption
  }

  implicit val intEq: Eq[Int] = new Eq[Int] {
    def eqv(a: Int, b: Int) = a == b
  }

  implicit def evalEq[A: Eq]: Eq[Eval[A]] = Eval.catsEqForEval[A]

  implicit def intStateEq[A: Eq]: Eq[IntState[A]] = stateEq[Int, A]

  implicit def intStateArb[A: Arbitrary]: Arbitrary[IntState[A]] = stateArbitrary[Int, A]

  implicit def listWrapperArbitrary[A: Arbitrary]: Arbitrary[ListWrapper[A]] = ListWrapper.listWrapperArbitrary[A]

  implicit def freeTListStateArb[A : Arbitrary]: Arbitrary[FreeTListState[A]] = freeTArb[IntState, IntState, A]

  implicit def freeTArb[F[_], G[_]: Applicative, A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]], A: Arbitrary[A]): Arbitrary[FreeT[F, G, A]] =
    Arbitrary(freeTGen[F, G, A](4))

  implicit def freeTListWrapperEq[A](implicit A: Eq[A]): Eq[FreeTListWrapper[A]] = new Eq[FreeTListWrapper[A]] {
    def eqv(a: FreeTListWrapper[A], b: FreeTListWrapper[A]) = Eq[ListWrapper[A]].eqv(a.runM(identity), b.runM(identity))
  }

  implicit def freeTListOptionEq[A](implicit A: Eq[A], OF: MonadRec[Option]): Eq[FreeTListOption[A]] = new Eq[FreeTListOption[A]] {
    def eqv(a: FreeTListOption[A], b: FreeTListOption[A]) = Eq[Option[A]].eqv(a.runM(_.list.headOption), b.runM(_.list.headOption))
  }

  implicit def freeTListStateEq[A](implicit A: Eq[A]): Eq[FreeTListState[A]] = new Eq[FreeTListState[A]] {
    def eqv(a: FreeTListState[A], b: FreeTListState[A]) = Eq[IntState[A]].eqv(a.runM(identity), b.runM(identity))
  }

  private def freeTGen[F[_], G[_]: Applicative, A](maxDepth: Int)(implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]], A: Arbitrary[A]): Gen[FreeT[F, G, A]] = {
    val noFlatMapped = Gen.oneOf(
      A.arbitrary.map(FreeT.pure[F, G, A]),
      F.arbitrary.map(FreeT.liftF[F, G, A])
    )

    val nextDepth = Gen.chooseNum(1, maxDepth - 1)

    def withFlatMapped = for {
      fDepth <- nextDepth
      freeDepth <- nextDepth
      f <- arbFunction1[A, FreeT[F, G, A]](Arbitrary(freeTGen[F, G, A](fDepth))).arbitrary
      freeFGA <- freeTGen[F, G, A](freeDepth)
    } yield freeFGA.flatMap(f)

    if (maxDepth <= 1) noFlatMapped
    else Gen.oneOf(noFlatMapped, withFlatMapped)
  }
  
}
