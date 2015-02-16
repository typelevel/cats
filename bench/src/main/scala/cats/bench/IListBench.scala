package cats.bench

import java.util.concurrent.TimeUnit

import cats.bench.input._
import cats.data.IList
import cats.std.int._
import org.openjdk.jmh.annotations._


@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class IListBench {

  val fixList: List[Int] = (1 to 100).toList
  val fixIList: IList[Int] = IList(1 to 100: _*)


  @Benchmark def mapIList(in: IListInput): IList[Int] =
    in.iList.map(_ + 1)

  @Benchmark def mapList(in: ListInput): List[Int] =
    in.list.map(_ + 1)

  @Benchmark def takeIList(in: IListInput, i: IndexInput): IList[Int] =
    in.iList.take(i.index)

  @Benchmark def takeList(in: ListInput, i: IndexInput): List[Int] =
    in.list.take(i.index)

  @Benchmark def dropIList(in: IListInput, i: IndexInput): IList[Int] =
    in.iList.drop(i.index)

  @Benchmark def dropList(in: ListInput, i: IndexInput): List[Int] =
    in.list.drop(i.index)

  @Benchmark def lookupIList(in: IListInput, i: IndexInput): Option[Int] =
    in.iList.lookup(i.index)

  @Benchmark def lookUpList(in: ListInput, i: IndexInput): Option[Int] =
    try {
      Some(in.list.apply(i.index))
    } catch {
      case _: Exception => None
    }

  @Benchmark def lastOptionIList(in: IListInput): Option[Int] =
    in.iList.lastOption

  @Benchmark def lastOptionList(in: ListInput): Option[Int] =
    in.list.lastOption

@Benchmark def appendIList(in1: IListInput, in2: IListInput): IList[Int] =
  in1.iList ::: in2.iList

  @Benchmark def appendList(in1: ListInput, in2: ListInput): List[Int] =
    in1.list ::: in2.list

  @Benchmark def flatMapIList(in: IListInput): IList[Int] =
    in.iList.flatMap(i => IList(i - 1, i, i + 1))

  @Benchmark def flatMapList(in: ListInput): List[Int] =
    in.list.flatMap(i => List(i - 1, i, i + 1))

  @Benchmark def filterIList(in: IListInput): IList[Int] =
    in.iList.filter(_ % 2 == 0)

  @Benchmark def filterList(in: ListInput): List[Int] =
    in.list.filter(_ % 2 == 0)

  @Benchmark def applyIList(in: ArrayInput): IList[Int] =
    IList(in.array: _*)

  @Benchmark def applyList(in: ArrayInput): List[Int] =
    List(in.array: _*)

  @Benchmark def eqIList(in1: IListInput, in2: IListInput): Boolean =
    in1.iList === in2.iList

  @Benchmark def eqList(in1: ListInput, in2: ListInput): Boolean =
    in1.list.equals(in2.list)

  @Benchmark def sizeIList(in: IListInput): Int =
    in.iList.size

  @Benchmark def sizeList(in: ListInput): Int =
    in.list.size

  @Benchmark def headOptionIList: Option[Int] =
    fixIList.headOption

  @Benchmark def headOptionList: Option[Int] =
    fixList.headOption

  @Benchmark def reverseIList(in: IListInput): IList[Int] =
    in.iList.reverse

  @Benchmark def reverseList(in: ListInput): List[Int] =
    in.list.reverse

}
