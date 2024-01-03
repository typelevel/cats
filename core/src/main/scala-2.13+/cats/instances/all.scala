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

abstract class AllInstancesBinCompat
    extends AllInstances
    with AllInstancesBinCompat0
    with AllInstancesBinCompat1
    with AllInstancesBinCompat2
    with AllInstancesBinCompat3
    with AllInstancesBinCompat4
    with AllInstancesBinCompat5
    with AllInstancesBinCompat6
    with AllInstancesBinCompat7
    with AllInstancesBinCompat8
    with AllInstancesBinCompat9

trait AllInstances
    extends AnyValInstances
    with ArraySeqInstances
    with BigIntInstances
    with BigDecimalInstances
    with BitSetInstances
    with EitherInstances
    with EqInstances
    with EquivInstances
    with FunctionInstances
    with FutureInstances
    with HashInstances
    with InvariantMonoidalInstances
    with LazyListInstances
    with ListInstances
    with MapInstances
    with OptionInstances
    with OrderInstances
    with OrderingInstances
    with ParallelInstances
    with PartialOrderInstances
    with PartialOrderingInstances
    with QueueInstances
    with SetInstances
    with SortedMapInstances
    with SortedSetInstances
    with ShowInstances
    with StreamInstances
    with StringInstances
    with SymbolInstances
    with TailRecInstances
    with TryInstances
    with TupleInstances
    with UUIDInstances
    with VectorInstances
    with PartialFunctionInstances

trait AllInstancesBinCompat0 extends FunctionInstancesBinCompat0 with Tuple2InstancesBinCompat0

trait AllInstancesBinCompat1
    extends OptionInstancesBinCompat0
    with ListInstancesBinCompat0
    with VectorInstancesBinCompat0
    with StreamInstancesBinCompat0
    with MapInstancesBinCompat0
    with SortedMapInstancesBinCompat0

trait AllInstancesBinCompat2 extends DurationInstances with FiniteDurationInstances

trait AllInstancesBinCompat3 extends AllCoreDurationInstances

trait AllInstancesBinCompat4 extends SortedMapInstancesBinCompat1 with MapInstancesBinCompat1

trait AllInstancesBinCompat5 extends SortedSetInstancesBinCompat0

trait AllInstancesBinCompat6 extends SortedSetInstancesBinCompat1 with SortedMapInstancesBinCompat2

trait AllInstancesBinCompat7 extends SeqInstances

trait AllInstancesBinCompat8 extends InvariantInstances

trait AllInstancesBinCompat9 extends DeadlineInstances

trait AllInstancesBinCompat10 extends InvariantInstancesBinCompat0
