package cats
package jvm
package tests

import cats.tests.CatsSuite

class EvalAsyncTests extends CatsSuite {
  test("evalAsync should be stack-safe") {
    import data.Streaming
    val ones = List(Eval.now(1),
                    Eval.later(1),
                    Eval.always(1),
                    EvalAsync[Int](_(1)))

    val onesStream: Streaming[Eval[Int]] = Streaming.continually(Streaming.fromList(ones)).flatMap(x => x)


    def taskMap2[A,B,C](t1: Eval[A], t2: Eval[B])(f: (A,B) => C): Eval[C] = {
      t1.flatMap(a => t2.map(b => f(a,b)))
    }

    def sequenceStreaming[A](fa: Streaming[Eval[A]]): Eval[Streaming[A]] = {
      fa.foldRight(Eval.later(Eval.now(Streaming.empty[A])))((a, st) =>
        st.map(b => taskMap2(b,a)((x,y) => Streaming.cons(y,x)))).value
    }

    val howmany = 1000000

    sequenceStreaming(onesStream.take(howmany)).value.foldLeft(0)((x, _) => x + 1) should be (howmany)

    onesStream.take(howmany).sequence.value.foldLeft(0)((x, _) => x + 1) should be (howmany)
  }

  test("EvalAsync should run forked tasks on another thread") {
    import EvalAsync._

    val pool = new java.util.concurrent.ForkJoinPool

    var time1: Long = 0
    var time2: Long = 0

    val t1: Eval[Unit] = Eval.later {
      Thread.sleep(2000)
      time1 = System.currentTimeMillis
    }.fork.run(pool)

    val t2: Eval[Unit] = Eval.later {
      Thread.sleep(1000)
      time2 = System.currentTimeMillis
      ()
    }.fork.run(pool)

    val cdl = new java.util.concurrent.CountDownLatch(2)

    t1.asyncValue(_ => cdl.countDown).run(pool)
    t2.asyncValue(_ => cdl.countDown).run(pool)

    time1 should be(0L)
    time2 should be(0L)

    cdl.await

    time2 should be > 0L
    time1 should be > time2
  }
}
