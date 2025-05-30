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
package instances

abstract private[instances] class AbstractAnyValInstances extends AbstractTupleInstances with AnyValInstances

trait AnyValInstances
    extends IntInstances
    with ByteInstances
    with CharInstances
    with LongInstances
    with ShortInstances
    with FloatInstances
    with DoubleInstances
    with BooleanInstances
    with UnitInstances
    with TupleInstances

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
