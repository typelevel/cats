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

package object instances {
  object all extends AllInstancesBinCompat
  object bigInt extends BigIntInstances
  object bigDecimal extends BigDecimalInstances
  object bitSet extends BitSetInstances
  object boolean extends BooleanInstances
  object byte extends ByteInstances
  object char extends CharInstances
  object double extends DoubleInstances
  object duration extends CoreDurationInstances with DurationInstances
  object either extends EitherInstances
  object eq extends EqInstances
  object equiv extends EquivInstances
  object float extends FloatInstances
  object finiteDuration extends CoreFiniteDurationInstances with FiniteDurationInstances
  object deadline extends DeadlineInstances
  object function extends FunctionInstances with FunctionInstancesBinCompat0
  object partialFunction extends PartialFunctionInstances

  /**
   * @deprecated
   *   Any non-pure use of [[scala.concurrent.Future Future]] with Cats is error prone
   *   (particularly the semantics of [[cats.Traverse#traverse traverse]] with regard to execution order are unspecified).
   *   We recommend using [[https://typelevel.org/cats-effect/ Cats Effect `IO`]] as a replacement for ''every'' use case of [[scala.concurrent.Future Future]].
   *   However, at this time there are no plans to remove these instances from Cats.
   *
   * @see [[https://github.com/typelevel/cats/issues/4176 Changes in Future traverse behavior between 2.6 and 2.7]]
   */
  object future extends FutureInstances

  object int extends IntInstances
  object invariant extends InvariantMonoidalInstances with InvariantInstances with InvariantInstancesBinCompat0
  object list extends ListInstances with ListInstancesBinCompat0
  object long extends LongInstances
  object option extends OptionInstances with OptionInstancesBinCompat0
  object map extends MapInstances with MapInstancesBinCompat0 with MapInstancesBinCompat1
  object order extends OrderInstances
  object ordering extends OrderingInstances
  object parallel extends ParallelInstances
  object partialOrder extends PartialOrderInstances
  object partialOrdering extends PartialOrderingInstances
  object queue extends QueueInstances
  object set extends SetInstances
  object seq extends SeqInstances
  object short extends ShortInstances
  object show extends ShowInstances
  object sortedMap
      extends SortedMapInstances
      with SortedMapInstancesBinCompat0
      with SortedMapInstancesBinCompat1
      with SortedMapInstancesBinCompat2
  object sortedSet extends SortedSetInstances with SortedSetInstancesBinCompat0 with SortedSetInstancesBinCompat1
  object stream extends StreamInstances with StreamInstancesBinCompat0
  object string extends StringInstances
  object tailRec extends TailRecInstances
  object try_ extends TryInstances
  object tuple extends TupleInstances with Tuple2InstancesBinCompat0
  object unit extends UnitInstances
  object uuid extends UUIDInstances
  object vector extends VectorInstances with VectorInstancesBinCompat0
}
