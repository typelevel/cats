package cats.bench.input

import org.openjdk.jmh.annotations.{Setup, Param, Scope, State}

@State(Scope.Thread)
class ArrayInput extends InputHelper {
  @Param(Array("10", "100", "1000"))
  var size: Int = _

  var array: Array[Int] = _

  @Setup
  def setup(): Unit =
    array = genArray(size)(r.nextInt())
}
