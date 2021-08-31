package cats.bench

import cats.Functor
import cats.data.EitherK
import cats.implicits._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class EitherKMapBench {
  def map1[F[_], G[_], A, B](e: EitherK[F, G, A])(f: A => B)(implicit F: Functor[F], G: Functor[G]): EitherK[F, G, B] =
    EitherK(e.run.bimap(F.lift(f), G.lift(f)))

  def map2[F[_], G[_], A, B](e: EitherK[F, G, A])(f: A => B)(implicit F: Functor[F], G: Functor[G]): EitherK[F, G, B] =
    EitherK(
      e.run match {
        case Right(ga) => Right(G.map(ga)(f))
        case Left(fa)  => Left(F.map(fa)(f))
      }
    )

  type EK = EitherK[List, Option, Int]

  val value1 = EitherK[List, Option, Int](Right(Some(0)))
  val value2 = EitherK[List, Option, Int](Right(None))
  val value3 = EitherK[List, Option, Int](Left(List(1, 2)))
  val f: Int => Int = _ + 1

  @Benchmark
  def incMap1: (EK, EK, EK) = (map1(value1)(f), map1(value2)(f), map1(value3)(f))

  @Benchmark
  def incMap2: (EK, EK, EK) = (map2(value1)(f), map2(value2)(f), map2(value3)(f))
}
