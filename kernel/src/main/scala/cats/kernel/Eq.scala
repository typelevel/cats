package cats.kernel

import java.util.UUID
import scala.collection.immutable.{BitSet, Queue, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.math.Equiv
import scala.{specialized => sp}
import scala.util.{Failure, Success, Try}
import compat.scalaVersionSpecific._

/**
 * A type class used to determine equality between 2 instances of the same
 * type. Any 2 instances `x` and `y` are equal if `eqv(x, y)` is `true`.
 * Moreover, `eqv` should form an equivalence relation.
 */
trait Eq[@sp A] extends Any with Serializable { self =>

  /**
   * Returns `true` if `x` and `y` are equivalent, `false` otherwise.
   */
  def eqv(x: A, y: A): Boolean

  /**
   * Returns `false` if `x` and `y` are equivalent, `true` otherwise.
   */
  def neqv(x: A, y: A): Boolean = !eqv(x, y)
}

abstract class EqFunctions[E[T] <: Eq[T]] {
  def eqv[@sp A](x: A, y: A)(implicit ev: E[A]): Boolean =
    ev.eqv(x, y)

  def neqv[@sp A](x: A, y: A)(implicit ev: E[A]): Boolean =
    ev.neqv(x, y)

}

trait EqToEquivConversion {

  /**
   * Implicitly derive a `scala.math.Equiv[A]` from a `Eq[A]`
   * instance.
   */
  implicit def catsKernelEquivForEq[A](implicit ev: Eq[A]): Equiv[A] =
    new Equiv[A] {
      def equiv(a: A, b: A) = ev.eqv(a, b)
    }
}

@suppressUnusedImportWarningForScalaVersionSpecific
object Eq
    extends EqFunctions[Eq]
    with EqToEquivConversion
    with ScalaVersionSpecificOrderInstances
    with instances.TupleOrderInstances
    with PartialOrderInstances {

  /**
   * Access an implicit `Eq[A]`.
   */
  @inline final def apply[A](implicit ev: Eq[A]): Eq[A] = ev

  /**
   * Convert an implicit `Eq[B]` to an `Eq[A]` using the given
   * function `f`.
   */
  def by[@sp A, @sp B](f: A => B)(implicit ev: Eq[B]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = ev.eqv(f(x), f(y))
    }

  /**
   * Return an Eq that gives the result of the and of eq1 and eq2
   * note this is idempotent
   */
  def and[@sp A](eq1: Eq[A], eq2: Eq[A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = eq1.eqv(x, y) && eq2.eqv(x, y)
    }

  /**
   * Return an Eq that gives the result of the or of this and that
   * Note this is idempotent
   */
  def or[@sp A](eq1: Eq[A], eq2: Eq[A]): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = eq1.eqv(x, y) || eq2.eqv(x, y)
    }

  /**
   * Create an `Eq` instance from an `eqv` implementation.
   */
  def instance[A](f: (A, A) => Boolean): Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = f(x, y)
    }

  /**
   * An `Eq[A]` that delegates to universal equality (`==`).
   *
   * This can be useful for case classes, which have reasonable `equals`
   * implementations
   */
  def fromUniversalEquals[A]: Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = x == y
    }

  /**
   * Everything is the same
   */
  def allEqual[A]: Eq[A] =
    new Eq[A] {
      def eqv(x: A, y: A) = true
    }

  /**
   * This is a monoid that creates an Eq that
   * checks that all equality checks pass
   */
  def allEqualBoundedSemilattice[A]: BoundedSemilattice[Eq[A]] =
    new BoundedSemilattice[Eq[A]] {
      def empty = allEqual[A]
      def combine(e1: Eq[A], e2: Eq[A]): Eq[A] = Eq.and(e1, e2)
      override def combineAllOption(es: IterableOnce[Eq[A]]): Option[Eq[A]] =
        if (es.iterator.isEmpty) None
        else {
          val materialized = es.iterator.toVector
          Some(new Eq[A] {
            def eqv(x: A, y: A) = materialized.forall(_.eqv(x, y))
          })
        }
    }

  /**
   * This is a monoid that creates an Eq that
   * checks that at least one equality check passes
   */
  def anyEqualSemilattice[A]: Semilattice[Eq[A]] =
    new Semilattice[Eq[A]] {
      def combine(e1: Eq[A], e2: Eq[A]): Eq[A] = Eq.or(e1, e2)
      override def combineAllOption(es: IterableOnce[Eq[A]]): Option[Eq[A]] =
        if (es.iterator.isEmpty) None
        else {
          val materialized = es.iterator.toVector
          Some(new Eq[A] {
            def eqv(x: A, y: A) = materialized.exists(_.eqv(x, y))
          })
        }
    }

  implicit def catsKernelInstancesForBitSet: PartialOrder[BitSet] with Hash[BitSet] =
    cats.kernel.instances.bitSet.catsKernelStdOrderForBitSet
  implicit def catsKernelPartialOrderForSet[A]: PartialOrder[Set[A]] =
    cats.kernel.instances.set.catsKernelStdPartialOrderForSet[A]
  implicit def catsKernelOrderForEither[A: Order, B: Order]: Order[Either[A, B]] =
    cats.kernel.instances.either.catsStdOrderForEither[A, B]

  implicit def catsKernelInstancesForUnit: Order[Unit] with Hash[Unit] =
    cats.kernel.instances.unit.catsKernelStdOrderForUnit
  implicit def catsKernelInstancesForBoolean: Order[Boolean] with Hash[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  implicit def catsKernelInstancesForByte: Order[Byte] with Hash[Byte] =
    cats.kernel.instances.byte.catsKernelStdOrderForByte
  implicit def catsKernelInstancesForShort: Order[Short] with Hash[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  implicit def catsKernelInstancesForInt: Order[Int] with Hash[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt
  implicit def catsKernelInstancesForLong: Order[Long] with Hash[Long] =
    cats.kernel.instances.long.catsKernelStdOrderForLong
  implicit def catsKernelInstancesForBigInt: Order[BigInt] with Hash[BigInt] =
    cats.kernel.instances.bigInt.catsKernelStdOrderForBigInt
  implicit def catsKernelInstancesForBigDecimal: Order[BigDecimal] with Hash[BigDecimal] =
    cats.kernel.instances.bigDecimal.catsKernelStdOrderForBigDecimal
  implicit def catsKernelInstancesForDuration: Order[Duration] with Hash[Duration] =
    cats.kernel.instances.duration.catsKernelStdOrderForDuration
  implicit def catsKernelInstancesForFiniteDuration: Order[FiniteDuration] with Hash[FiniteDuration] =
    cats.kernel.instances.all.catsKernelStdOrderForFiniteDuration
  implicit def catsKernelInstancesForChar: Order[Char] with Hash[Char] =
    cats.kernel.instances.char.catsKernelStdOrderForChar
  implicit def catsKernelInstancesForSymbol: Order[Symbol] with Hash[Symbol] =
    cats.kernel.instances.symbol.catsKernelStdOrderForSymbol
  implicit def catsKernelInstancesForString: Order[String] with Hash[String] =
    cats.kernel.instances.string.catsKernelStdOrderForString
  implicit def catsKernelInstancesForUUID: Order[UUID] with Hash[UUID] =
    cats.kernel.instances.uuid.catsKernelStdOrderForUUID
  implicit def catsKernelInstancesForDouble: Order[Double] with Hash[Double] =
    cats.kernel.instances.double.catsKernelStdOrderForDouble
  implicit def catsKernelInstancesForFloat: Order[Float] with Hash[Float] =
    cats.kernel.instances.float.catsKernelStdOrderForFloat

  implicit def catsKernelOrderForOption[A: Order]: Order[Option[A]] =
    cats.kernel.instances.option.catsKernelStdOrderForOption[A]
  implicit def catsKernelOrderForList[A: Order]: Order[List[A]] =
    cats.kernel.instances.list.catsKernelStdOrderForList[A]
  implicit def catsKernelOrderForVector[A: Order]: Order[Vector[A]] =
    cats.kernel.instances.vector.catsKernelStdOrderForVector[A]
  implicit def catsKernelOrderForQueue[A: Order]: Order[Queue[A]] =
    cats.kernel.instances.queue.catsKernelStdOrderForQueue[A]
  implicit def catsKernelOrderForSortedSet[A: Order]: Order[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdOrderForSortedSet[A]
  implicit def catsKernelOrderForFunction0[A: Order]: Order[() => A] =
    cats.kernel.instances.function.catsKernelOrderForFunction0[A]

  /**
   * you may wish to do equality by making `implicit val eqT: Eq[Throwable] = Eq.allEqual`
   * doing a fine grained equality on Throwable can make the code very execution
   * order dependent
   */
  implicit def catsStdEqForTry[A, T](implicit A: Eq[A], T: Eq[Throwable]): Eq[Try[A]] =
    new Eq[Try[A]] {
      def eqv(x: Try[A], y: Try[A]): Boolean =
        (x, y) match {
          case (Success(a), Success(b)) => A.eqv(a, b)
          case (Failure(a), Failure(b)) => T.eqv(a, b)
          case _                        => false
        }
    }
}

private[kernel] trait PartialOrderInstances extends HashInstances {
  implicit def catsKernelPartialOrderForOption[A: PartialOrder]: PartialOrder[Option[A]] =
    cats.kernel.instances.option.catsKernelStdPartialOrderForOption[A]
  implicit def catsKernelPartialOrderForList[A: PartialOrder]: PartialOrder[List[A]] =
    cats.kernel.instances.list.catsKernelStdPartialOrderForList[A]
  implicit def catsKernelPartialOrderForVector[A: PartialOrder]: PartialOrder[Vector[A]] =
    cats.kernel.instances.vector.catsKernelStdPartialOrderForVector[A]
  implicit def catsKernelPartialOrderForQueue[A: PartialOrder]: PartialOrder[Queue[A]] =
    cats.kernel.instances.queue.catsKernelStdPartialOrderForQueue[A]
  implicit def catsKernelPartialOrderForFunction0[A: PartialOrder]: PartialOrder[() => A] =
    cats.kernel.instances.function.catsKernelPartialOrderForFunction0[A]
}

private[kernel] trait HashInstances extends EqInstances {
  implicit def catsKernelHashForSet[A]: Hash[Set[A]] = cats.kernel.instances.set.catsKernelStdHashForSet[A]
  implicit def catsKernelHashForOption[A: Hash]: Hash[Option[A]] =
    cats.kernel.instances.option.catsKernelStdHashForOption[A]
  implicit def catsKernelHashForList[A: Hash]: Hash[List[A]] = cats.kernel.instances.list.catsKernelStdHashForList[A]
  implicit def catsKernelHashForVector[A: Hash]: Hash[Vector[A]] =
    cats.kernel.instances.vector.catsKernelStdHashForVector[A]
  implicit def catsKernelHashForQueue[A: Hash]: Hash[Queue[A]] =
    cats.kernel.instances.queue.catsKernelStdHashForQueue[A]
  implicit def catsKernelHashForSortedSet[A: Hash]: Hash[SortedSet[A]] =
    cats.kernel.instances.sortedSet.catsKernelStdHashForSortedSet[A](Hash[A])
  implicit def catsKernelHashForFunction0[A: Hash]: Hash[() => A] =
    cats.kernel.instances.function.catsKernelHashForFunction0[A]
  implicit def catsKernelHashForMap[K: Hash, V: Hash]: Hash[Map[K, V]] =
    cats.kernel.instances.map.catsKernelStdHashForMap[K, V]
  implicit def catsKernelHashForSortedMap[K: Order: Hash, V: Hash]: Hash[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdHashForSortedMap[K, V]
  implicit def catsKernelHashForEither[A: Hash, B: Hash]: Hash[Either[A, B]] =
    cats.kernel.instances.either.catsStdHashForEither[A, B]
}

private[kernel] trait EqInstances {
  implicit def catsKernelEqForOption[A: Eq]: Eq[Option[A]] = cats.kernel.instances.option.catsKernelStdEqForOption[A]
  implicit def catsKernelEqForList[A: Eq]: Eq[List[A]] = cats.kernel.instances.list.catsKernelStdEqForList[A]
  implicit def catsKernelEqForVector[A: Eq]: Eq[Vector[A]] = cats.kernel.instances.vector.catsKernelStdEqForVector[A]
  implicit def catsKernelEqForQueue[A: Eq]: Eq[Queue[A]] = cats.kernel.instances.queue.catsKernelStdEqForQueue[A]
  implicit def catsKernelEqForFunction0[A: Eq]: Eq[() => A] = cats.kernel.instances.function.catsKernelEqForFunction0[A]
  implicit def catsKernelEqForMap[K, V: Eq]: Eq[Map[K, V]] = cats.kernel.instances.map.catsKernelStdEqForMap[K, V]
  implicit def catsKernelEqForSortedMap[K: Order, V: Eq]: Eq[SortedMap[K, V]] =
    cats.kernel.instances.sortedMap.catsKernelStdEqForSortedMap[K, V]
  implicit def catsKernelEqForEither[A: Eq, B: Eq]: Eq[Either[A, B]] =
    cats.kernel.instances.either.catsStdEqForEither[A, B]
}
