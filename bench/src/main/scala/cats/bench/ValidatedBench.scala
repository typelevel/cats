package cats.bench

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class ValidatedBench {
  val x: Validated[String, Int] = Validated.valid(100)

  def bimapPointfree[E, A, EE, AA](v: Validated[E, A])(fe: E => EE, fa: A => AA): Validated[EE, AA] =
    v.fold(fe.andThen(Invalid.apply), fa.andThen(Valid.apply))

  def bimapPointfull[E, A, EE, AA](v: Validated[E, A])(fe: E => EE, fa: A => AA): Validated[EE, AA] =
    v match {
      case Valid(a)   => Valid(fa(a))
      case Invalid(e) => Invalid(fe(e))
    }

  @Benchmark
  def pointfull: Validated[Int, Int] = bimapPointfull(x)(_.length, _ + 1)
  @Benchmark
  def pointfree: Validated[Int, Int] = bimapPointfree(x)(_.length, _ + 1)
}
