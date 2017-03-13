package cats
package jvm

import data.Reader
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object EvalAsync {
  def apply[A](cb: (A => Unit) => Unit): Eval[A] = Eval.always {
    val cdl = new CountDownLatch(1)
    var result: Option[A] = None
    cb((a: A) => {result = Some(a); cdl.countDown})
    cdl.await
    result.get // YOLO
  }

  implicit class EvalFork[A](val eval: Eval[A]) extends AnyVal {
    def callable(cb: A => Unit): Callable[Unit] = new Callable[Unit] {
      def call = cb(eval.value)
    }

    /**
     * Returns an Eval that will produce the same value as the wrapped
     * Eval, but extracting the value from the resulting Eval will
     * submit the work to the given ExecutorService
     */
    def fork: Reader[ExecutorService, Eval[A]] =
      Reader { pool =>
        EvalAsync { cb =>
          val _ = pool.submit(callable(cb))
        }
      }

    /**
     * Run this computation asynchronously, and call the callback with the result
     */
    final def asyncValue(cb: A => Unit): Reader[ExecutorService, Unit] =
      Reader { pool =>
        val _ = pool.submit(callable(cb))
      }
  }
}
