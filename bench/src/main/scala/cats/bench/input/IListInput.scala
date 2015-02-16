package cats.bench.input

import cats.data.IList
import org.openjdk.jmh.annotations.{Setup, Param, Scope, State}

@State(Scope.Thread)
class IListInput extends InputHelper {
  @Param(Array("10", "100", "1000"))
  var size: Int = _

  var iList: IList[Int] = _

  @Setup
  def setup(): Unit =
    iList = IList(genArray(size)(r.nextInt()): _*)
}
