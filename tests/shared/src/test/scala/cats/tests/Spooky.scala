package cats.tests

/**
 * Class for spooky side-effects and action-at-a-distance.
 *
 * It is basically a mutable counter that can be used to measure how
 * many times an otherwise pure function is being evaluated.
 */
class Spooky(var counter: Int = 0) {
  def increment(): Unit = counter += 1
}
