package cats
package kernel
package compat

private[kernel] class HashCompat {
  // Adapted from scala.util.hashing.MurmurHash#productHash.
  private[kernel] def product1HashWithPrefix(_1Hash: Int, prefix: String): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, prefix.hashCode)
    h = mix(h, _1Hash)
    finalizeHash(h, 1)
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  private[cats] def product2HashWithPrefix(_1Hash: Int, _2Hash: Int, prefix: String): Int = {
    import scala.util.hashing.MurmurHash3._
    var h = productSeed
    h = mix(h, prefix.hashCode)
    h = mix(h, _1Hash)
    h = mix(h, _2Hash)
    finalizeHash(h, 2)
  }

  private[cats] def updateUnorderedHashC(c: Int, h: Int): Int = c * (h | 1)

  // adapted from [[scala.util.hashing.MurmurHash3]],
  // but modified standard `Any#hashCode` to `ev.hash`.
  def listHash[A](x: List[A])(implicit A: Hash[A]): Int = {
    import scala.util.hashing.MurmurHash3.{finalizeHash, mix, rangeHash, seqSeed}
    var n = 0
    var h = seqSeed
    var rangeState = 0 // 0 = no data, 1 = first elem read, 2 = has valid diff, 3 = invalid
    var rangeDiff = 0
    var prev = 0
    var initial = 0
    var elems = x
    while (!elems.isEmpty) {
      val head = elems.head
      val tail = elems.tail
      val hash = A.hash(head)
      h = mix(h, hash)
      rangeState match {
        case 0 =>
          initial = hash
          rangeState = 1
        case 1 =>
          rangeDiff = hash - prev
          rangeState = 2
        case 2 =>
          if (rangeDiff != hash - prev) rangeState = 3
        case _ =>
      }
      prev = hash
      n += 1
      elems = tail
    }
    if (rangeState == 2) rangeHash(initial, rangeDiff, prev, seqSeed)
    else finalizeHash(h, n)
  }

  // adapted from scala.util.hashing.MurmurHash3
  def orderedHash[A](xs: TraversableOnce[A])(implicit A: Hash[A]): Int = {
    import scala.util.hashing.MurmurHash3.{finalizeHash, mix, seqSeed}

    val it = xs.iterator
    var h = seqSeed
    // scalastyle:off
    if (!it.hasNext) return finalizeHash(h, 0)
    // scalastyle:on
    val x0 = it.next()
    // scalastyle:off
    if (!it.hasNext) return finalizeHash(mix(h, A.hash(x0)), 1)
    // scalastyle:on
    val x1 = it.next()

    val initial = A.hash(x0)
    h = mix(h, initial)
    val h0 = h
    var prev = A.hash(x1)
    val rangeDiff = prev - initial
    var i = 2
    while (it.hasNext) {
      h = mix(h, prev)
      val hash = A.hash(it.next())
      if (rangeDiff != hash - prev) {
        h = mix(h, hash)
        i += 1
        while (it.hasNext) {
          h = mix(h, A.hash(it.next()))
          i += 1
        }
        // scalastyle:off
        return finalizeHash(h, i)
        // scalastyle:on
      }
      prev = hash
      i += 1
    }
    avalanche(mix(mix(h0, rangeDiff), prev))
  }

  // adapted from scala.util.hashing.MurmurHash3
  /** Force all bits of the hash to avalanche. Used for finalizing the hash. */
  final protected def avalanche(hash: Int): Int = {
    var h = hash

    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16

    h
  }
}
