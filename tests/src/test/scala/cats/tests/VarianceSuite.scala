package cats.tests

import cats.{Bifunctor, Contravariant, Functor}
import cats.implicits._
import cats.implicits.variance._

class VarianceSuite extends CatsSuite {

  sealed trait Foo
  case class Bar(x: Int) extends Foo
  case object Baz extends Foo

  test("Auto-variance should infer subtypes correctly") {
    def shouldInfer[F[_]: Functor](fi: F[Int]) =
      fi.map(i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_]: Functor](fi: F[Int]): F[Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should infer supertypes correctly") {
    def shouldCompile[F[_]: Contravariant](fi: F[Foo])(f: F[Bar] => Int) =
      f(fi)
  }

  test("Auto-variance should widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]) =
      fi.bimap(i => if (true) Left(Bar(i)) else Right(Baz), i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should left widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]) =
      fi.bimap(identity, i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Int, Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should right widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]) =
      fi.bimap(i => if (true) Left(Bar(i)) else Right(Baz), identity)

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Int] = shouldInfer[F](fi)
  }

}
