package cats
package std

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

trait IntInstances {

  implicit val intShow: Show[Int] =
    Show.fromToString[Int]

  implicit val intAlgebra: Group[Int] with Order[Int] =
    new Group[Int] with Order[Int] {
      def empty: Int = 0
      def inverse(a: Int): Int = -a
      def combine(x: Int, y: Int): Int = x+y
      def compare(x: Int, y: Int): Int =
        if (x < y) -1 else if (x > y) 1 else 0
    }

}

trait ByteInstances {

  implicit val byteShow: Show[Byte] =
    Show.fromToString[Byte]

  implicit val byteOrder: Order[Byte] =
    new Order[Byte] {
      def compare(x: Byte, y: Byte): Int =
        if (x < y) -1 else if (y < x) 1 else 0
    }
}

trait CharInstances {

  implicit val charShow: Show[Char] =
    Show.fromToString[Char]

  implicit val charOrder: Order[Char] =
    new Order[Char] {
      def compare(x: Char, y: Char): Int =
        if (x < y) -1 else if (y < x) 1 else 0
    }
}

trait ShortInstances {

  implicit val shortShow: Show[Short] =
    Show.fromToString[Short]

  implicit val shortOrder: Order[Short] =
    new Order[Short] {
      def compare(x: Short, y: Short): Int =
        if (x < y) -1 else if (y < x) 1 else 0
    }

}

trait LongInstances {

  implicit val longShow: Show[Long] =
    Show.fromToString[Long]

  implicit val longOrder: Order[Long] =
    new Order[Long] {
      def compare(x: Long, y: Long): Int =
        if (x < y) -1 else if (y < x) 1 else 0
    }
}

trait FloatInstances {

  implicit val floatShow: Show[Float] =
    Show.fromToString[Float]

  implicit val floatOrder: Order[Float] =
    new Order[Float] {
      def compare(x: Float, y: Float): Int =
        java.lang.Float.compare(x, y)
    }

}

trait DoubleInstances {

  implicit val doubleShow: Show[Double] =
    Show.fromToString[Double]

  implicit val doubleOrder: Order[Double] =
    new Order[Double] {
      def compare(x: Double, y: Double): Int =
        java.lang.Double.compare(x, y)
    }

}

trait BooleanInstances {

  implicit val booleanShow: Show[Boolean] =
    Show.fromToString[Boolean]

  implicit val booleanOrder: Order[Boolean] =
    new Order[Boolean] {
      def compare(x: Boolean, y: Boolean): Int =
        if (x == y) 0 else if (x) 1 else -1
    }

}

trait UnitInstances {

  implicit val unitShow: Show[Unit] =
    Show.fromToString[Unit]

  implicit val unitOrder: Order[Unit] =
    new Order[Unit] {
      def compare(x: Unit, y: Unit): Int = 0
    }

}
