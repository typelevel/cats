/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats

import java.util.UUID
import scala.collection.immutable.{BitSet, Queue, Seq, SortedMap, SortedSet}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Try

/**
 * A type class to provide textual representation. It is meant to be a
 * better "toString". Whereas toString exists for any Object,
 * regardless of whether or not the creator of the class explicitly
 * made a toString method, a Show instance will only exist if someone
 * explicitly provided one.
 */
trait Show[T] extends Show.ContravariantShow[T]

/**
 * Hand rolling the type class boilerplate due to scala/bug#6260 and scala/bug#10458
 */
object Show extends ScalaVersionSpecificShowInstances with ShowInstances {

  def apply[A](implicit instance: Show[A]): Show[A] = instance

  trait ContravariantShow[-T] extends Serializable {
    def show(t: T): String
  }

  trait Ops[A] {
    def typeClassInstance: Show[A]
    def self: A
    def show: String = typeClassInstance.show(self)
  }

  trait ToShowOps {
    implicit def toShow[A](target: A)(implicit tc: Show[A]): Ops[A] =
      new Ops[A] {
        val self = target
        val typeClassInstance = tc
      }
  }

  /**
   * creates an instance of [[Show]] using the provided function
   */
  def show[A](f: A => String): Show[A] = f(_)

  /**
   * creates an instance of [[Show]] using object toString
   */
  def fromToString[A]: Show[A] = _.toString

  final case class Shown(override val toString: String) extends AnyVal
  object Shown {
    implicit def mat[A](x: A)(implicit z: ContravariantShow[A]): Shown = Shown(z.show(x))
  }

  final case class ShowInterpolator(_sc: StringContext) extends AnyVal {
    def show(args: Shown*): String = _sc.s(args: _*)
  }

  implicit val catsContravariantForShow: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](fa: Show[A])(f: B => A): Show[B] = b => fa.show(f(b))
  }

  implicit def catsShowForUnit: Show[Unit] = cats.instances.unit.catsStdShowForUnit
  implicit def catsShowForBoolean: Show[Boolean] = cats.instances.boolean.catsStdShowForBoolean
  implicit def catsShowForByte: Show[Byte] = cats.instances.byte.catsStdShowForByte
  implicit def catsShowForShort: Show[Short] = cats.instances.short.catsStdShowForShort
  implicit def catsShowForInt: Show[Int] = cats.instances.int.catsStdShowForInt
  implicit def catsShowForLong: Show[Long] = cats.instances.long.catsStdShowForLong
  implicit def catsShowForFloat: Show[Float] = cats.instances.float.catsStdShowForFloat
  implicit def catsShowForDouble: Show[Double] = cats.instances.double.catsStdShowForDouble
  implicit def catsShowForBigInt: Show[BigInt] = cats.instances.bigInt.catsStdShowForBigInt
  implicit def catsShowForBigDecimal: Show[BigDecimal] = cats.instances.bigDecimal.catsStdShowForBigDecimal
  implicit def catsShowForChar: Show[Char] = cats.instances.char.catsStdShowForChar
  implicit def catsShowForSymbol: Show[Symbol] = cats.instances.symbol.catsStdShowForSymbol
  implicit def catsShowForString: Show[String] = cats.instances.string.catsStdShowForString
  implicit def catsShowForUUID: Show[UUID] = cats.instances.uuid.catsStdShowForUUID
  implicit def catsShowForDuration: Show[Duration] = cats.instances.duration.catsStdShowForDurationUnambiguous
  implicit def catsShowForBitSet: Show[BitSet] = cats.instances.bitSet.catsStdShowForBitSet

  implicit def catsShowForOption[A: Show]: Show[Option[A]] = cats.instances.option.catsStdShowForOption[A]
  implicit def catsShowForTry[A: Show]: Show[Try[A]] = cats.instances.try_.catsStdShowForTry[A]
  implicit def catsShowForList[A: Show]: Show[List[A]] = cats.instances.list.catsStdShowForList[A]
  implicit def catsShowForVector[A: Show]: Show[Vector[A]] = cats.instances.vector.catsStdShowForVector[A]
  implicit def catsShowForQueue[A: Show]: Show[Queue[A]] = cats.instances.queue.catsStdShowForQueue[A]
  implicit def catsShowForEither[A: Show, B: Show]: Show[Either[A, B]] =
    cats.instances.either.catsStdShowForEither[A, B]
  implicit def catsShowForSortedMap[K: Show, V: Show]: Show[SortedMap[K, V]] =
    cats.instances.sortedMap.catsStdShowForSortedMap[K, V]

  @deprecated("Use catsStdShowForTuple2 in cats.instances.NTupleShowInstances", "2.4.0")
  def catsShowForTuple2[A: Show, B: Show]: Show[(A, B)] = cats.instances.tuple.catsStdShowForTuple2[A, B]
}

private[cats] trait ShowInstances extends cats.instances.NTupleShowInstances with ShowInstances0 {
  implicit def catsShowForFiniteDuration: Show[FiniteDuration] =
    cats.instances.finiteDuration.catsStdShowForFiniteDurationUnambiguous

  implicit def catsShowForSortedSet[A: Show]: Show[SortedSet[A]] = cats.instances.sortedSet.catsStdShowForSortedSet[A]
}

private[cats] trait ShowInstances0 {
  implicit def catsShowForSeq[A: Show]: Show[Seq[A]] = cats.instances.seq.catsStdShowForSeq[A]
  implicit def catsShowForMap[K: Show, V: Show]: Show[Map[K, V]] = cats.instances.map.catsStdShowForMap[K, V]
  implicit def catsShowForSet[A: Show]: Show[Set[A]] = cats.instances.set.catsStdShowForSet[A]
}
