package cats.tests

import cats.Eval
import cats.data.ContT
import cats.kernel.Eq
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import cats.syntax.eq._
import org.scalacheck.Prop._

class ContTSuite extends CatsSuite {

  implicit def arbContT[M[_], A, B](implicit arbFn: Arbitrary[(B => M[A]) => M[A]]): Arbitrary[ContT[M, A, B]] =
    Arbitrary(arbFn.arbitrary.map(ContT[M, A, B](_)))

  implicit def eqContT[M[_], A, B](implicit arbFn: Arbitrary[B => M[A]], eqMA: Eq[M[A]]): Eq[ContT[M, A, B]] = {
    val genItems = Gen.listOfN(100, arbFn.arbitrary)
    val fns = genItems.sample.get
    new Eq[ContT[M, A, B]] {
      def eqv(a: ContT[M, A, B], b: ContT[M, A, B]) =
        fns.forall { fn =>
          eqMA.eqv(a.run(fn), b.run(fn))
        }
    }
  }

  checkAll("ContT[Function0, Int, *]", MonadTests[ContT[Function0, Int, *]].monad[Int, String, Int])
  checkAll("ContT[Eval, Int, *]", MonadTests[ContT[Eval, Int, *]].monad[Int, String, Int])
  checkAll("ContT[Function0, Int, *]", DeferTests[ContT[Function0, Int, *]].defer[Int])

  /**
   * c.mapCont(f).run(g) == f(c.run(g))
   */
  def mapContLaw[M[_], A, B](implicit
    eqma: Eq[M[A]],
    cArb: Arbitrary[ContT[M, A, B]],
    fnArb: Arbitrary[M[A] => M[A]],
    gnArb: Arbitrary[B => M[A]]
  ) =
    forAll { (cont: ContT[M, A, B], fn: M[A] => M[A], gn: B => M[A]) =>
      assert(eqma.eqv(cont.mapCont(fn).run(gn), fn(cont.run(gn))))
    }

  /**
   * cont.withCont(f).run(g) == cont.run(f(g))
   */
  def withContLaw[M[_], A, B, C](implicit
    eqma: Eq[M[A]],
    cArb: Arbitrary[ContT[M, A, B]],
    fnArb: Arbitrary[(C => M[A]) => B => M[A]],
    gnArb: Arbitrary[C => M[A]]
  ) =
    forAll { (cont: ContT[M, A, B], fn: (C => M[A]) => B => M[A], gn: C => M[A]) =>
      assert(eqma.eqv(cont.withCont(fn).run(gn), cont.run(fn(gn))))
    }

  test("ContT.mapContLaw[Function0, Int, String]") {
    mapContLaw[Function0, Int, String]
  }

  test("ContT.mapContLaw[Eval, Int, String]") {
    mapContLaw[Eval, Int, String]
  }

  test("ContT.withContLaw[Function0, Int, String, Int]") {
    withContLaw[Function0, Int, String, Int]
  }

  test("ContT.withContLaw[Eval, Int, String, Int]") {
    withContLaw[Eval, Int, String, Int]
  }

  test("ContT.defer defers evaluation until run is invoked") {
    forAll { (b: Int, cb: Int => Eval[String]) =>
      var didSideEffect = false

      val contT = ContT.defer[Eval, String, Int] {
        didSideEffect = true
        b
      }
      assert(didSideEffect === (false))

      contT.run(cb)
      assert(didSideEffect === (true))
    }
  }

}
