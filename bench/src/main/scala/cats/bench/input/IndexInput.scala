package cats.bench.input

import org.openjdk.jmh.annotations.{Scope, Setup, State}

@State(Scope.Thread)
class IndexInput extends InputHelper {

  var index: Int = _

  @Setup
  def setup(): Unit =
    index = r.nextInt(1000)
}
