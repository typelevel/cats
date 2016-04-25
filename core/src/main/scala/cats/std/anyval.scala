package cats
package std

import cats.kernel.CommutativeGroup

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

trait IntInstances extends cats.kernel.std.IntInstances {

  implicit val intShow: Show[Int] =
    Show.fromToString[Int]

  implicit val intGroup: CommutativeGroup[Int] =
    new CommutativeGroup[Int] {
      def combine(x: Int, y: Int): Int = x + y
      def empty: Int = 0
      def inverse(x: Int): Int = -x
      override def remove(x: Int, y: Int): Int = x - y
    }
}

trait ByteInstances extends cats.kernel.std.ByteInstances {

  implicit val byteShow: Show[Byte] =
    Show.fromToString[Byte]

  implicit val byteGroup: CommutativeGroup[Byte] =
    new CommutativeGroup[Byte] {
      def combine(x: Byte, y: Byte): Byte = (x + y).toByte
      def empty: Byte = 0
      def inverse(x: Byte): Byte = (-x).toByte
      override def remove(x: Byte, y: Byte): Byte = (x - y).toByte
    }
}

trait CharInstances extends cats.kernel.std.CharInstances {
  implicit val charShow: Show[Char] =
    Show.fromToString[Char]
}

trait ShortInstances extends cats.kernel.std.ShortInstances {

  implicit val shortShow: Show[Short] =
    Show.fromToString[Short]

  implicit val shortGroup: CommutativeGroup[Short] =
    new CommutativeGroup[Short] {
      def combine(x: Short, y: Short): Short = (x + y).toShort
      def empty: Short = 0
      def inverse(x: Short): Short = (-x).toShort
      override def remove(x: Short, y: Short): Short = (x - y).toShort
    }
}

trait LongInstances extends cats.kernel.std.LongInstances {

  implicit val longShow: Show[Long] =
    Show.fromToString[Long]

  implicit val longGroup: CommutativeGroup[Long] =
    new CommutativeGroup[Long] {
      def combine(x: Long, y: Long): Long = x + y
      def empty: Long = 0L
      def inverse(x: Long): Long = -x
      override def remove(x: Long, y: Long): Long = x - y
    }
}

trait FloatInstances extends cats.kernel.std.FloatInstances {

  implicit val floatShow: Show[Float] =
    Show.fromToString[Float]

  implicit val floatGroup: CommutativeGroup[Float] =
    new CommutativeGroup[Float] {
      def combine(x: Float, y: Float): Float = x + y
      def empty: Float = 0F
      def inverse(x: Float): Float = -x
      override def remove(x: Float, y: Float): Float = x - y
    }
}

trait DoubleInstances extends cats.kernel.std.DoubleInstances {

  implicit val doubleShow: Show[Double] =
    Show.fromToString[Double]

  implicit val doubleGroup: CommutativeGroup[Double] =
    new CommutativeGroup[Double] {
      def combine(x: Double, y: Double): Double = x + y
      def empty: Double = 0D
      def inverse(x: Double): Double = -x
      override def remove(x: Double, y: Double): Double = x - y
    }
}

trait BooleanInstances extends cats.kernel.std.BooleanInstances {

  implicit val booleanShow: Show[Boolean] =
    Show.fromToString[Boolean]
}

trait UnitInstances extends cats.kernel.std.UnitInstances {

  implicit val unitShow: Show[Unit] =
    Show.fromToString[Unit]
}
