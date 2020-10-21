package cats.tests

import cats._
import cats.data.Kleisli

// TODO should be fixed in Dotty 3.0.0-M1
// <https://github.com/lampepfl/dotty/issues/9793>
class KleisliSuiteScala2 extends CatsSuite {
  test("Functor[Kleisli[F, Int, *]] is not ambiguous when an ApplicativeError and a FlatMap are in scope for F") {
    def shouldCompile1[F[_], E](implicit F: ApplicativeError[F, E], FM: FlatMap[F]): Functor[Kleisli[F, Int, *]] =
      Functor[Kleisli[F, Int, *]]
  }
}
