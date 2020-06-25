package cats.tests

import cats.arrow.Profunctor
import cats.conversions.all._
import cats.implicits._
import cats.{Bifunctor, Contravariant, Functor}

class VarianceSuite extends CatsSuite {

  sealed trait FooBar
  sealed trait Foo extends FooBar
  case class Bar(x: Int) extends Foo
  case object Baz extends Foo

  test("Auto-variance should infer subtypes correctly") {
    def shouldInfer[F[_]: Functor](fi: F[Int]): F[Either[Bar, Baz.type]] =
      fi.map(i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_]: Functor](fi: F[Int]): F[Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should infer supertypes correctly") {
    def shouldCompile[F[_]: Contravariant](fi: F[Foo])(f: F[Bar] => Int) =
      f(fi)
  }

  test("Auto-variance should widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Either[Bar, Baz.type], Either[Bar, Baz.type]] =
      fi.bimap(i => if (true) Left(Bar(i)) else Right(Baz), i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should left widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Int, Either[Bar, Baz.type]] =
      fi.bimap(identity, i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Int, Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should right widen a bifunctor automatically") {
    def shouldInfer[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Either[Bar, Baz.type], Int] =
      fi.bimap(i => if (true) Left(Bar(i)) else Right(Baz), identity)

    def inferred[F[_, _]: Bifunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Int] = shouldInfer[F](fi)
  }

  test("Auto-variance should widen a profunctor automatically") {
    def shouldInfer[F[_, _]: Profunctor](fi: F[Int, Int]): F[Either[FooBar, FooBar], Either[Bar, Baz.type]] =
      fi.dimap((_: Either[FooBar, FooBar]) => 1)(i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Profunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should widen the second type parameter of a profunctor automatically") {
    def shouldInfer[F[_, _]: Profunctor](fi: F[Int, Int]): F[Int, Either[Bar, Baz.type]] =
      fi.dimap(identity[Int])(i => if (true) Left(Bar(i)) else Right(Baz))

    def inferred[F[_, _]: Profunctor](fi: F[Int, Int]): F[Int, Either[Foo, Foo]] = shouldInfer[F](fi)
  }

  test("Auto-variance should narrow the first type parameter of a profunctor automatically") {
    def shouldInfer[F[_, _]: Profunctor](fi: F[Int, Int]): F[Either[FooBar, FooBar], Int] =
      fi.dimap((_: Either[FooBar, FooBar]) => 1)(identity[Int])

    def inferred[F[_, _]: Profunctor](fi: F[Int, Int]): F[Either[Foo, Foo], Int] = shouldInfer[F](fi)
  }

}
