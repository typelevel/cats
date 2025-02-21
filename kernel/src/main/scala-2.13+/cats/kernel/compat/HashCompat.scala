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
package kernel
package compat

/* This file is derived in part from https://github.com/scala/scala/blob/v2.13.0-RC1/src/library/scala/util/hashing/MurmurHash3.scala
 * Modified by Typelevel for redistribution in Cats.
 *
 * Copyright EPFL and Lightbend, Inc.
 * Scala
 * Copyright (c) 2002-2022 EPFL
 * Copyright (c) 2011-2022 Lightbend, Inc.
 *
 * Scala includes software developed at
 * LAMP/EPFL (https://lamp.epfl.ch/) and
 * Lightbend, Inc. (https://www.lightbend.com/).
 *
 * Licensed under the Apache License, Version 2.0 (the "License").
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

private[kernel] class HashCompat {
  // Adapted from scala.util.hashing.MurmurHash#productHash.
  private[kernel] def product1HashWithPrefix(_1Hash: Int, prefix: String): Int = {
    import scala.util.hashing.MurmurHash3.*
    var h = productSeed
    h = mix(h, prefix.hashCode)
    h = mix(h, _1Hash)
    finalizeHash(h, 1)
  }

  // Adapted from scala.util.hashing.MurmurHash#productHash.
  private[cats] def product2HashWithPrefix(_1Hash: Int, _2Hash: Int, prefix: String): Int = {
    import scala.util.hashing.MurmurHash3.*
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
          if (rangeDiff != hash - prev || rangeDiff == 0) rangeState = 3
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
  def orderedHash[A](xs: IterableOnce[A])(implicit A: Hash[A]): Int = {
    import scala.util.hashing.MurmurHash3.{finalizeHash, mix, seqSeed}

    val it = xs.iterator
    var h = seqSeed
    if (!it.hasNext) return finalizeHash(h, 0)
    val x0 = it.next()
    if (!it.hasNext) return finalizeHash(mix(h, A.hash(x0)), 1)
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
      if (rangeDiff != hash - prev || rangeDiff == 0) {
        h = mix(h, hash)
        i += 1
        while (it.hasNext) {
          h = mix(h, A.hash(it.next()))
          i += 1
        }
        return finalizeHash(h, i)
      }
      prev = hash
      i += 1
    }
    avalanche(mix(mix(h0, rangeDiff), prev))
  }

  // adapted from scala.util.hashing.MurmurHash3
  /**
   * Force all bits of the hash to avalanche. Used for finalizing the hash.
   */
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
