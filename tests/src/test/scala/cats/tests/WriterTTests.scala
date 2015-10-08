package cats
package tests

import cats.data.WriterT
import cats.laws.discipline._
import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._

import org.scalacheck.Prop.forAll

class WriterTTests extends CatsSuite {
  checkAll("WriterT[List, String, Int]", MonadTests[WriterT[List, String, ?]].monad[String, Int, Int])

  test("double swap is a noop"){
    forAll { w: WriterT[List, String, Int] =>
      w.swap.swap should === (w)
    }
  }

  test("reset on pure is a noop"){
    forAll { i: Int =>
      val w = Monad[WriterT[List, String, ?]].pure(i)
      w should === (w.reset)
    }
  }

  test("reset consistencey"){
    forAll { (i: Int, w1: WriterT[Id, String, Int], w2: WriterT[Id, String, Int]) =>
      // if the value is the same, everything should be the same
      w1.map(_ => i).reset should === (w2.map(_ => i).reset)
    }
  }
}
