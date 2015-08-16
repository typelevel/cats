package cats
package task

import scala.math.pow
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

import java.util.concurrent.atomic.AtomicInteger

import cats.tests.{CatsProps, CatsSuite}

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.BooleanOperators

class NondeterminismCheck extends CatsProps {

  import cats.std.int._
  import cats.task.std.future._
  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val nf: Nondeterminism[Future] = futureNondeterminism

  def setup(ns: List[Int]): (List[Future[Int]], AtomicInteger, AtomicInteger) = {
    val total = new AtomicInteger(0)
    val count = new AtomicInteger(0)
    def sideEffects(n: Int): Future[Int] =
      Future { total.addAndGet(n); count.addAndGet(1); n }
    (ns.map(sideEffects), total, count)
  }

  def verify[A](ns: List[Int], work: List[Future[Int]] => Future[A], expected: A): Unit = {
    val (futures, total, count) = setup(ns)
    val future = work(futures)
    val result = Await.result(future, Duration("1s"))
    result shouldBe expected
    total.get shouldBe ns.sum
    count.get shouldBe ns.size
  }

  property("combineAll") {
    forAll { (ns: List[Int]) =>
      verify(ns, fns => nf.combineAll(fns), ns.sum)
    }
  }

  property("unorderedGather") {
    forAll { (ns: List[Int]) =>
      verify(ns, fns => nf.unorderedGather(fns).map(_.toSet), ns.toSet)
    }
  }

  property("orderedGather") {
    forAll { (ns: List[Int]) =>
      verify(ns, fns => nf.orderedGather(fns), ns)
    }
  }

  property("asyncMap2") {
    forAll { (x: Int, y: Int) =>
      verify(List(x, y), { case List(fx, fy) => nf.asyncMap2(fx, fy)(_ * _) }, x * y)
    }
  }

  property("foldFirst2") {
    forAll { (x: Int, y: Int) =>
      val (List(fx, fy), _, _) = setup(List(x, y))
      val future = nf.foldFirst2(fx, fy)(_ * 2, _ * 3)
      val result = Await.result(future, Duration("1s"))
      result should (equal(x * 2) or equal(y * 3))
    }
  }
}
