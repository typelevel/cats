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
  with    BigIntInstances

trait IntInstances extends algebra.std.IntInstances {

  implicit val intShow: Show[Int] = new Show[Int] {
    def show(f: Int): String = f.toString
  }

}

trait ByteInstances /* missing algebra typeclasses */ {

  implicit val byteShow: Show[Byte] = new Show[Byte] {
    def show(f: Byte): String = f.toString
  }

}

trait CharInstances /* missing algebra typeclasses */ {

  implicit val charShow: Show[Char] = new Show[Char] {
    def show(f: Char): String = f.toString
  }

}

trait ShortInstances /* missing algebra typeclasses */ {

  implicit val shortShow: Show[Short] = new Show[Short] {
    def show(f: Short): String = f.toString
  }

}

trait LongInstances /* missing algebra typeclasses */ {

  implicit val longShow: Show[Long] = new Show[Long] {
    def show(f: Long): String = f.toString
  }

}

trait FloatInstances /* missing algebra typeclasses */ {

  implicit val floatShow: Show[Float] = new Show[Float] {
    def show(f: Float): String = f.toString
  }

}

trait DoubleInstances /* missing algebra typeclasses */ {

  implicit val doubleShow: Show[Double] = new Show[Double] {
    def show(f: Double): String = f.toString
  }

}

trait BooleanInstances extends algebra.std.BooleanInstances {

  implicit val booleanShow: Show[Boolean] = new Show[Boolean] {
    def show(f: Boolean): String = f.toString
  }

}

trait BigIntInstances extends algebra.std.BigIntInstances {

  implicit val bigIntShow: Show[BigInt] = new Show[BigInt] {
    def show(f: BigInt): String = f.toString
  }

}

trait UnitInstances /* missing algebra typeclasses */ {

  implicit val unitShow: Show[Unit] = new Show[Unit] {
    def show(f: Unit): String = ().toString
  }

}
