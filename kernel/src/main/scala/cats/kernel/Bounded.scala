package cats.kernel

import java.util.UUID
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.{specialized => sp}

/**
 * A type class used to name the lower limit of a type.
 */
trait LowerBounded[@sp A] {
  def partialOrder: PartialOrder[A]

  /**
   * Returns the lower limit of a type.
   */
  def minBound: A
}

trait LowerBoundedFunctions[L[T] <: LowerBounded[T]] {
  def minBound[@sp A](implicit ev: L[A]): A = ev.minBound
}

object LowerBounded extends LowerBoundedFunctions[LowerBounded] {
  @inline def apply[A](implicit l: LowerBounded[A]): LowerBounded[A] = l

  implicit def catsKernelLowerBoundedForUnit: LowerBounded[Unit] = cats.kernel.instances.unit.catsKernelStdOrderForUnit
  implicit def catsKernelLowerBoundedForBoolean: LowerBounded[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  implicit def catsKernelLowerBoundedForByte: LowerBounded[Byte] = cats.kernel.instances.byte.catsKernelStdOrderForByte
  implicit def catsKernelLowerBoundedForInt: LowerBounded[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt
  implicit def catsKernelLowerBoundedForShort: LowerBounded[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  implicit def catsKernelLowerBoundedForLong: LowerBounded[Long] = cats.kernel.instances.long.catsKernelStdOrderForLong
  implicit def catsKernelLowerBoundedForDuration: LowerBounded[Duration] =
    cats.kernel.instances.duration.catsKernelStdOrderForDuration
  implicit def catsKernelLowerBoundedForFiniteDuration: LowerBounded[FiniteDuration] =
    cats.kernel.instances.all.catsKernelStdOrderForFiniteDuration
  implicit def catsKernelLowerBoundedForChar: LowerBounded[Char] = cats.kernel.instances.char.catsKernelStdOrderForChar
  implicit def catsKernelLowerBoundedForString: LowerBounded[String] =
    cats.kernel.instances.string.catsKernelStdOrderForString
  implicit def catsKernelLowerBoundedForSymbol: LowerBounded[Symbol] =
    cats.kernel.instances.symbol.catsKernelStdOrderForSymbol
  implicit def catsKernelLowerBoundedForUUID: LowerBounded[UUID] = cats.kernel.instances.uuid.catsKernelStdOrderForUUID
}

/**
 * A type class used to name the upper limit of a type.
 */
trait UpperBounded[@sp A] {
  def partialOrder: PartialOrder[A]

  /**
   * Returns the upper limit of a type.
   */
  def maxBound: A
}

trait UpperBoundedFunctions[U[T] <: UpperBounded[T]] {
  def maxBound[@sp A](implicit ev: U[A]): A = ev.maxBound
}

object UpperBounded extends UpperBoundedFunctions[UpperBounded] {
  @inline def apply[A](implicit u: UpperBounded[A]): UpperBounded[A] = u

  implicit def catsKernelUpperBoundedForUnit: UpperBounded[Unit] = cats.kernel.instances.unit.catsKernelStdOrderForUnit
  implicit def catsKernelUpperBoundedForBoolean: UpperBounded[Boolean] =
    cats.kernel.instances.boolean.catsKernelStdOrderForBoolean
  implicit def catsKernelUpperBoundedForByte: UpperBounded[Byte] = cats.kernel.instances.byte.catsKernelStdOrderForByte
  implicit def catsKernelUpperBoundedForInt: UpperBounded[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt
  implicit def catsKernelUpperBoundedForShort: UpperBounded[Short] =
    cats.kernel.instances.short.catsKernelStdOrderForShort
  implicit def catsKernelUpperBoundedForLong: UpperBounded[Long] = cats.kernel.instances.long.catsKernelStdOrderForLong
  implicit def catsKernelUpperBoundedForDuration: UpperBounded[Duration] =
    cats.kernel.instances.duration.catsKernelStdOrderForDuration
  implicit def catsKernelUpperBoundedForFiniteDuration: UpperBounded[FiniteDuration] =
    cats.kernel.instances.all.catsKernelStdOrderForFiniteDuration
  implicit def catsKernelUpperBoundedForChar: UpperBounded[Char] = cats.kernel.instances.char.catsKernelStdOrderForChar
  implicit def catsKernelUpperBoundedForUUID: UpperBounded[UUID] = cats.kernel.instances.uuid.catsKernelStdOrderForUUID
}
