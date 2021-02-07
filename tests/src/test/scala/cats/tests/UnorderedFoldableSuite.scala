package cats.tests

import cats.UnorderedFoldable
import cats.kernel.CommutativeMonoid
import cats.laws.discipline.UnorderedFoldableTests
import cats.syntax.unorderedFoldable._
import org.scalacheck.Arbitrary
import cats.syntax.eq._
import org.scalacheck.Prop._

sealed abstract class UnorderedFoldableSuite[F[_]](name: String)(implicit
  ArbFString: Arbitrary[F[String]],
  ArbFInt: Arbitrary[F[Int]]
) extends CatsSuite {

  def iterator[T](fa: F[T]): Iterator[T]
  def specializedUnorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B

  private[this] val instance: UnorderedFoldable[F] =
    new UnorderedFoldable[F] {
      def unorderedFoldMap[A, B: CommutativeMonoid](fa: F[A])(f: A => B): B =
        specializedUnorderedFoldMap(fa)(f)
    }

  test(s"UnorderedFoldable[$name].isEmpty") {
    forAll { (fa: F[String]) =>
      assert(instance.isEmpty(fa) === (instance.size(fa) === 0L))
    }
  }

  test(s"UnorderedFoldable[$name].nonEmpty") {
    forAll { (fa: F[String]) =>
      assert(instance.nonEmpty(fa) === (instance.size(fa) > 0L))
    }
  }

  test(s"UnorderedFoldable[$name].count") {
    forAll { (fa: F[String], p: String => Boolean) =>
      implicit val F: UnorderedFoldable[F] = instance
      assert(fa.count(p) === (iterator(fa).count(p).toLong))
    }
  }

  test(s"UnorderedFoldable[$name].size") {
    forAll { (fa: F[String]) =>
      implicit val F: UnorderedFoldable[F] = instance
      assert(fa.count(Function.const(true)) === (fa.size))
    }
  }
  checkAll("F[Int]", UnorderedFoldableTests[F](instance).unorderedFoldable[Int, Int])
}

final class UnorderedFoldableSetSuite extends UnorderedFoldableSuite[Set]("set") {
  def iterator[T](set: Set[T]): Iterator[T] = set.iterator
  def specializedUnorderedFoldMap[A, B: CommutativeMonoid](fa: Set[A])(f: A => B): B =
    UnorderedFoldable[Set].unorderedFoldMap(fa)(f)
}

final class UnorderedFoldableMapSuite extends UnorderedFoldableSuite[Map[String, *]]("map") {
  def iterator[T](map: Map[String, T]): Iterator[T] = map.valuesIterator
  def specializedUnorderedFoldMap[A, B: CommutativeMonoid](fa: Map[String, A])(f: A => B): B =
    UnorderedFoldable[Map[String, *]].unorderedFoldMap(fa)(f)
}

sealed abstract class SpecializedUnorderedFoldableSuite[F[_]: UnorderedFoldable](name: String)(implicit
  ArbFString: Arbitrary[F[String]]
) extends CatsSuite {

  def iterator[T](fa: F[T]): Iterator[T]

  test(s"Specialized UnorderedFoldable[$name].count") {
    forAll { (fa: F[String], p: String => Boolean) =>
      assert(fa.count(p) === (iterator(fa).count(p).toLong))
    }
  }
}

final class SpecializedUnorderedFoldableSetSuite extends SpecializedUnorderedFoldableSuite[Set]("set") {
  def iterator[T](set: Set[T]): Iterator[T] = set.iterator
}

final class SpecializedUnorderedFoldableMapSuite extends SpecializedUnorderedFoldableSuite[Map[String, *]]("map") {
  def iterator[T](map: Map[String, T]): Iterator[T] = map.valuesIterator
}
