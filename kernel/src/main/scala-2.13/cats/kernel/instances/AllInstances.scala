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

package cats.kernel
package instances

trait AllInstances
    extends ArraySeqInstances
    with BigDecimalInstances
    with BigIntInstances
    with BitSetInstances
    with BooleanInstances
    with ByteInstances
    with CharInstances
    with DoubleInstances
    with EqInstances
    with EitherInstances
    with DurationInstances
    with FloatInstances
    with FunctionInstances
    with HashInstances
    with IntInstances
    with LazyListInstances
    with ListInstances
    with LongInstances
    with MapInstances
    with OptionInstances
    with OrderInstances
    with PartialOrderInstances
    with QueueInstances
    with SetInstances
    with SeqInstances
    with ShortInstances
    with StreamInstances
    with StringInstances
    with SymbolInstances
    with TupleInstances
    with UnitInstances
    with UUIDInstances
    with VectorInstances

private[instances] trait AllInstancesBinCompat0 extends FiniteDurationInstances

private[instances] trait AllInstancesBinCompat1 extends SortedMapInstances with SortedSetInstances

private[instances] trait AllInstancesBinCompat2 extends DeadlineInstances
