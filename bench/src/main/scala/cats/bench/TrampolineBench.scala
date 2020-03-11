package cats.bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import cats._
import cats.implicits._
import cats.free.Trampoline

import scala.util.control.TailCalls

@State(Scope.Benchmark)
class TrampolineBench {

  val N = 15

  @Benchmark
  def eval(): Int = evalFib(N).value

  def evalFib(n: Int): Eval[Int] =
    if (n < 2) Eval.now(n) else for {
      x <- Eval.defer(evalFib(n - 1))
      y <- Eval.defer(evalFib(n - 2))
    } yield x + y


  @Benchmark
  def trampoline(): Int = trampolineFib(N).run

  def trampolineFib(n: Int): Trampoline[Int] =
    if (n < 2) Trampoline.done(n) else for {
      x <- Trampoline.defer(trampolineFib(n - 1))
      y <- Trampoline.defer(trampolineFib(n - 2))
    } yield x + y

   @Benchmark
   def stdlib(): Int = stdlibFib(N).result

   def stdlibFib(n: Int): TailCalls.TailRec[Int] =
     if (n < 2) TailCalls.done(n) else for {
       x <- TailCalls.tailcall(stdlibFib(n - 1))
       y <- TailCalls.tailcall(stdlibFib(n - 2))
     } yield x + y
}
