package cats
package data

private[data] trait ChainCompat[+A] { self: Chain[A] =>

  /**
   * The number of elements in this chain, if it can be cheaply computed, -1 otherwise.
   * Cheaply usually means: Not requiring a collection traversal.
   */
  final def knownSize: Long =
    this match {
      case Chain.Empty        => 0
      case Chain.Singleton(_) => 1
      case Chain.Wrap(seq)    => seq.knownSize.toLong
      case _                  => -1
    }
}
