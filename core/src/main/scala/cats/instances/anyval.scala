package cats
package instances

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
  with    TupleInstances

trait IntInstances extends cats.kernel.instances.IntInstances {
  implicit val catsStdShowForInt: Show[Int] = Show.fromToString[Int]
}

trait ByteInstances extends cats.kernel.instances.ByteInstances {
  implicit val catsStdShowForByte: Show[Byte] = Show.fromToString[Byte]
}

trait CharInstances extends cats.kernel.instances.CharInstances {
  implicit val catsStdShowForChar: Show[Char] = Show.fromToString[Char]
}

trait ShortInstances extends cats.kernel.instances.ShortInstances {
  implicit val catsStdShowForShort: Show[Short] = Show.fromToString[Short]
}

trait LongInstances extends cats.kernel.instances.LongInstances {
  implicit val catsStdShowForLong: Show[Long] = Show.fromToString[Long]
}

trait FloatInstances extends cats.kernel.instances.FloatInstances {
  implicit val catsStdShowForFloat: Show[Float] = Show.fromToString[Float]
}

trait DoubleInstances extends cats.kernel.instances.DoubleInstances {
  implicit val catsStdShowForDouble: Show[Double] = Show.fromToString[Double]
}

trait BooleanInstances extends cats.kernel.instances.BooleanInstances {
  implicit val catsStdShowForBoolean: Show[Boolean] = Show.fromToString[Boolean]
}

trait UnitInstances extends cats.kernel.instances.UnitInstances {
  implicit val catsStdShowForUnit: Show[Unit] = Show.fromToString[Unit]
}
