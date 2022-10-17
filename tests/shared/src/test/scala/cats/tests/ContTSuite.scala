/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import java.util.concurrent.atomic.AtomicInteger

import cats.Eval
import cats.data.ContT
import cats.kernel.Eq
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import cats.syntax.all._
import org.scalacheck.Prop._

class ContTSuite extends CatsSuite {

  implicit def arbContT[M[_], A, B](implicit arbFn: Arbitrary[(B => M[A]) => M[A]]): Arbitrary[ContT[M, A, B]] =
    Arbitrary(arbFn.arbitrary.map(ContT[M, A, B](_)))

  implicit def eqContT[M[_], A, B](implicit arbFn: Arbitrary[B => M[A]], eqMA: Eq[M[A]]): Eq[ContT[M, A, B]] = {
    val genItems = Gen.listOfN(100, arbFn.arbitrary)
    val fns = genItems.sample.get
    (a, b) => fns.forall(fn => eqMA.eqv(a.run(fn), b.run(fn)))
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
      assert(didSideEffect === false)

      contT.run(cb)
      assert(didSideEffect === true)
    }
  }

  test("ContT.resetT and shiftT delimit continuations") {
    forAll { (cb: Unit => Eval[Unit]) =>
      val counter = new AtomicInteger(0)
      var a = 0
      var b = 0
      var c = 0
      var d = 0

      val contT: ContT[Eval, Unit, Unit] = ContT
        .resetT(
          ContT.shiftT { (k: Unit => Eval[Unit]) =>
            ContT.defer[Eval, Unit, Unit] {
              a = counter.incrementAndGet()
            } >>
              ContT.liftF(k(())) >>
              ContT.defer[Eval, Unit, Unit] {
                b = counter.incrementAndGet()
              }
          }
            >> ContT.defer[Eval, Unit, Unit] {
              c = counter.incrementAndGet()
            }
        )
        .flatMap { _ =>
          ContT.defer[Eval, Unit, Unit] {
            d = counter.incrementAndGet()
          }
        }

      contT.run(cb).value
      assert(a == 1)
      assert(b == 3)
      assert(c == 2)
      assert(d == 4)
    }
  }
  test("ContT.shiftT stack safety") {
    var counter = 0
    val maxIters = 50000

    def contT: ContT[Eval, Int, Int] =
      ContT.shiftT { (k: Int => Eval[Int]) =>
        ContT
          .defer[Eval, Int, Int] {
            counter = counter + 1
            counter
          }
          .flatMap { n =>
            if (n === maxIters) ContT.liftF(k(n)) else contT
          }
      }

    assert(contT.run(Eval.now(_)).value === maxIters)
  }

  test("ContT.resetT stack safety") {
    var counter = 0
    val maxIters = 50000

    def contT: ContT[Eval, Int, Int] =
      ContT.resetT(
        ContT
          .defer[Eval, Int, Int] {
            counter = counter + 1
            counter
          }
          .flatMap { n =>
            if (n === maxIters) ContT.pure[Eval, Int, Int](n) else contT
          }
      )

    assert(contT.run(Eval.now(_)).value === maxIters)
  }

  test("ContT.flatMap stack safety") {
    val maxIters = 20000
    var counter = 0

    def contT: ContT[Eval, Int, Int] =
      ContT
        .defer[Eval, Int, Int] {
          counter = counter + 1
          counter
        }
        .flatMap { n =>
          if (n === maxIters) ContT.pure[Eval, Int, Int](n) else contT
        }

    assert(contT.run(Eval.now(_)).value === maxIters)
  }

  // test from issue 2950
  test("ContT.map stack-safety") {
    val maxIters = 20000
    var k = ContT.defer[Eval, Int, Int](0)
    for (_ <- 1 to maxIters) {
      k = k.map(x => x + 1)
    }
    assert(k.run(_.pure[Eval]).value == maxIters)
  }

  test("ContT.callCC short-circuits and invokes the continuation") {
    forAll { (cb: Unit => Eval[Int]) =>
      var shouldNotChange = false
      var shouldChange = false
      var shouldAlsoChange = false

      val contT: ContT[Eval, Int, Unit] = for {
        _ <- ContT.callCC((k: Unit => ContT[Eval, Int, Unit]) =>
          ContT.defer[Eval, Int, Unit] {
            shouldChange = true
          } >>
            k(()) >>
            ContT.defer[Eval, Int, Unit] {
              shouldNotChange = true
            }
        )
        _ <- ContT.defer[Eval, Int, Unit] {
          shouldAlsoChange = true
        }
      } yield ()

      contT.run(cb).value

      assert(shouldNotChange === false)
      assert(shouldChange === true)
      assert(shouldAlsoChange === true)
    }
  }

  test("ContT.callCC stack-safety") {

    val counter = new AtomicInteger(0)
    val maxIters = 10000

    def contT: ContT[Eval, Unit, Int] = ContT
      .callCC { (k: Int => ContT[Eval, Unit, Int]) =>
        ContT
          .defer[Eval, Unit, Int] {
            counter.incrementAndGet()
          }
          .flatMap { n =>
            if (n === maxIters) ContT.pure[Eval, Unit, Int](n) else contT
          }
      }
      .flatMap { n =>
        ContT.pure[Eval, Unit, Int](n)
      }

    contT.run(_ => Eval.now(())).value
  }

}
