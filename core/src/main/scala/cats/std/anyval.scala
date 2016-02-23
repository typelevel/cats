package cats
package std

import algebra.CommutativeGroup
import algebra.ring.AdditiveCommutativeGroup

trait AnyValInstances
  extends IntInstances
  with    ByteInstances
  with    CharInstances
  with    LongInstances
  with    ShortInstances
  with    FloatInstances
  with    DoubleInstances
  with    BooleanInstances
  with    UnitInstances

trait IntInstances extends algebra.std.IntInstances {

  implicit val intShow: Show[Int] =
    Show.fromToString[Int]

  implicit val intGroup: CommutativeGroup[Int] =
    AdditiveCommutativeGroup[Int].additive

}

trait ByteInstances /* missing algebra type classes */ {

  implicit val byteShow: Show[Byte] =
    Show.fromToString[Byte]

  // TODO: replace this minimal algebra with one from the algebra project
  implicit val byteAlgebra: CommutativeGroup[Byte] with Order[Byte] =
    new CommutativeGroup[Byte] with Order[Byte] {
      def combine(x: Byte, y: Byte): Byte = (x + y).toByte
      def empty: Byte = 0
      def inverse(x: Byte): Byte = (-x).toByte
      def compare(x: Byte, y: Byte): Int =
        if (x < y) -1 else if (y < x) 1 else 0
    }
}

trait CharInstances /* missing algebra type classes */ {

  implicit val charShow: Show[Char] =
    Show.fromToString[Char]

  implicit val charOrder: Order[Char] =
    new Order[Char] {
      def compare(x: Char, y: Char): Int =
        if (x < y) -1 else if (y < x) 1 else 0
    }
}

trait ShortInstances /* missing algebra type classes */ {

  implicit val shortShow: Show[Short] =
    Show.fromToString[Short]

  // TODO: replace this minimal algebra with one from the algebra project
  implicit val shortAlgebra: CommutativeGroup[Short] with Order[Short] =
    new CommutativeGroup[Short] with Order[Short] {
      def combine(x: Short, y: Short): Short = (x + y).toShort
      def empty: Short = 0
      def inverse(x: Short): Short = (-x).toShort
      def compare(x: Short, y: Short): Int =
        if (x < y) -1 else if (y < x) 1 else 0
    }

}

trait LongInstances /* missing algebra type classes */ {

  implicit val longShow: Show[Long] =
    Show.fromToString[Long]

  // TODO: replace this minimal algebra with one from the algebra project
  implicit val longAlgebra: CommutativeGroup[Long] with Order[Long] =
    new CommutativeGroup[Long] with Order[Long] {
      def combine(x: Long, y: Long): Long = x + y
      def empty: Long = 0L
      def inverse(x: Long): Long = -x
      def compare(x: Long, y: Long): Int =
        if (x < y) -1 else if (y < x) 1 else 0
    }
}

trait FloatInstances /* missing algebra type classes */ {

  implicit val floatShow: Show[Float] =
    Show.fromToString[Float]

  // TODO: replace this minimal algebra with one from the algebra project
  implicit val floatAlgebra: CommutativeGroup[Float] with Order[Float] =
    new CommutativeGroup[Float] with Order[Float] {
      def combine(x: Float, y: Float): Float = x + y
      def empty: Float = 0F
      def inverse(x: Float): Float = -x
      def compare(x: Float, y: Float): Int =
        java.lang.Float.compare(x, y)
    }

}

trait DoubleInstances /* missing algebra type classes */ {

  implicit val doubleShow: Show[Double] =
    Show.fromToString[Double]

  // TODO: replace this minimal algebra with one from the algebra project
  implicit val doubleAlgebra: CommutativeGroup[Double] with Order[Double] =
    new CommutativeGroup[Double] with Order[Double] {
      def combine(x: Double, y: Double): Double = x + y
      def empty: Double = 0D
      def inverse(x: Double): Double = -x
      def compare(x: Double, y: Double): Int =
        java.lang.Double.compare(x, y)
    }

}

trait BooleanInstances extends algebra.std.BooleanInstances {

  implicit val booleanShow: Show[Boolean] =
    Show.fromToString[Boolean]

}

trait UnitInstances /* missing algebra type classes */ {

  implicit val unitShow: Show[Unit] =
    Show.fromToString[Unit]

  implicit val unitAlgebra: CommutativeGroup[Unit] with Order[Unit] =
    new CommutativeGroup[Unit] with Order[Unit] {
      def combine(x: Unit, y: Unit): Unit = ()
      def empty: Unit = ()
      def inverse(x: Unit): Unit = ()
      def compare(x: Unit, y: Unit): Int = 0
    }

}
