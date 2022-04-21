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

package cats.data

import cats.Always
import cats.CommutativeApplicative
import cats.Eval
import cats.Foldable
import cats.Semigroup
import cats.Show
import cats.UnorderedTraverse
import cats.kernel.CommutativeMonoid
import cats.kernel.CommutativeSemigroup
import cats.kernel.Monoid
import cats.kernel.Eq
import cats.kernel.Hash
import cats.kernel.instances.StaticMethods
import cats.syntax.eq._
import java.util.Arrays

/**
 * An immutable hash map using [[cats.kernel.Hash]] for hashing.
 *
 * Implemented using the CHAMP encoding.
 * @see [[https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf Efficient Immutable Collections]]
 *
 * @tparam K the type of the keys contained in this hash map.
 * @tparam V the type of the values contained in this hash map.
 * @param hashKey the [[cats.kernel.Hash]] instance used for hashing keys.
 */
final class HashMap[K, +V] private[data] (private[data] val rootNode: HashMap.Node[K, V])(implicit val hashKey: Hash[K])
    extends HashMapCompat[K, V] {

  /**
    * An iterator for this map that can be used only once.
    *
    * @return an iterator that iterates through the key-value pairs of this map.
    */
  final def iterator: Iterator[(K, V)] =
    new HashMap.Iterator(rootNode)

  /**
    * A reverse iterator for this map that can be used only once.
    *
    * @return an iterator that iterates through this map in the reverse order of [[HashMap#iterator]].
    */
  final def reverseIterator: Iterator[(K, V)] =
    new HashMap.ReverseIterator(rootNode)

  /**
    * The size of this map.
    *
    * @return the number of elements in this map.
    */
  final def size: Int = rootNode.size

  /**
    * Tests whether the map is empty.
    *
    * @return `true` if the map contains no elements, `false` otherwise.
    */
  final def isEmpty: Boolean = size == 0

  /**
    * Tests whether the map is not empty.
    *
    * @return `true` if the map contains at least one element, `false` otherwise.
    */
  final def nonEmpty: Boolean = !isEmpty

  /**
    * Apply `f` to each key-value pair for its side effects.
    *
    * @param f the function to apply to each key-value pair.
    */
  final def foreach[U](f: (K, V) => U): Unit =
    rootNode.foreach(f)

  /**
    * Test whether the map contains `key`.
    *
    * @param key the key to check for map membership.
    * @return `true` if the map contains `key`, `false` otherwise.
    */
  final def contains(key: K): Boolean =
    rootNode.contains(key, hashKey.hash(key), 0)

  /**
    * Get the value associated with `key` in this map.
    *
    * @param key the key to check for map membership.
    * @return A [[scala.Some]] containing the value if present, else [[scala.None]].
    */
  final def get(key: K): Option[V] =
    rootNode.get(key, hashKey.hash(key), 0)

  /**
    * Get the value associated with `key` in this map, or `default` if not present.
    *
    * @param key the key to check for map membership.
    * @param default the value to use in case `key` is not present.
    * @return the value if present, else `default`.
    */
  final def getOrElse[VV >: V](key: K, default: => VV): VV =
    get(key).getOrElse(default)

  /**
    * Creates a new map with an additional key-value pair, unless the key is already present,
    * in which case the value for `key` is replaced by `value`.
    *
    * @param key the key to be added.
    * @param value the value to be added.
    * @return a new map that contains all key-value pairs of this map and that also contains a mapping from `key` to `value`.
    */
  final def add[VV >: V](key: K, value: VV): HashMap[K, VV] = {
    val keyHash = hashKey.hash(key)
    val newRootNode = rootNode.add(key, keyHash, value, 0)

    if (newRootNode eq rootNode)
      this
    else
      new HashMap(newRootNode)
  }

  /**
    * Creates a new map with the given key removed from the map.
    *
    * @param key the key to be removed.
    * @return a new map that contains all elements of this map but that does not contain `key`.
    */
  final def remove(key: K): HashMap[K, V] = {
    val keyHash = hashKey.hash(key)
    val newRootNode = rootNode.remove(key, keyHash, 0)

    if (newRootNode eq rootNode)
      this
    else
      new HashMap(newRootNode)
  }

  /**
   * Typesafe equality operator.
   *
   * This method is similar to [[scala.Any#==]] except that it only allows two [[cats.data.HashMap]]
   * values of the same key-value type to be compared to each other, and uses equality provided
   * by [[cats.kernel.Eq]] instances, rather than using the universal equality provided by
   * [[java.lang.Object#equals]].
   *
   * @param that the [[cats.data.HashMap]] to check for equality with this map.
   * @param eqValue the [[cats.kernel.Eq]] instance to use for comparing values.
   * @return `true` if this map and `that` are equal, `false` otherwise.
   */
  final def ===[VV >: V](that: HashMap[K, VV])(implicit eqValue: Eq[VV]): Boolean =
    (this eq that) || (this.rootNode === that.rootNode)

  final override def equals(that: Any): Boolean = that match {
    case map: HashMap[_, _] =>
      (this eq map) || (this.rootNode == map.rootNode)
    case _ =>
      false
  }

  /**
   * Compute a hash code value for this map.
   *
   * This method is similar to [[java.lang.Object#hashCode]] except that it computes a hash code
   * according to [[cats.Hash]] instances.
   *
   * @param hashValue the [[cats.kernel.Hash]] instance to use for hashing values of type `VV`.
   * @return a hash code value for this map.
   */
  final def hash[VV >: V](implicit hashValue: Hash[VV]): Int =
    StaticMethods.unorderedHash(this.iterator: Iterator[(K, VV)])

  final override def hashCode(): Int = {
    implicit val valueHash = Hash.fromUniversalHashCode[V]
    StaticMethods.unorderedHash(this.iterator)
  }

  /**
   * Typesafe stringification operator.
   *
   * This method is similar to [[java.lang.Object#toString]] except that it stringifies values according
   * to [[cats.Show]] instances.
   *
   * @param showKey the [[cats.Show]] instance to use for showing keys of type `K`.
   * @param showValue the [[cats.Show]] instance to use for showing values of type `V`.
   * @return a [[java.lang.String]] representation of this map.
   */
  final def show[VV >: V](implicit showKey: Show[K], showValue: Show[VV]): String =
    iterator.map { case (k, v) => s"${showKey.show(k)} -> ${showValue.show(v)}" }.mkString("HashMap(", ", ", ")")

  final override def toString() =
    iterator.map { case (k, v) => s"$k -> $v" }.mkString("HashMap(", ", ", ")")
}

object HashMap extends HashMapInstances {

  /**
    * Creates a new empty [[cats.data.HashMap]] which uses `hashKey` for hashing.
    *
    * @param hashKey the [[cats.kernel.Hash]] instance used for hashing keys.
    * @return a new empty [[cats.data.HashMap]].
    */
  final def empty[K, V](implicit hashKey: Hash[K]): HashMap[K, V] =
    new HashMap[K, V](Node.empty[K, V])

  /**
  * Creates a new [[cats.data.HashMap]] which contains all elements of `kvs`.
  *
  * @param kvs the key-value pairs to add to the [[cats.data.HashMap]].
  * @param hashKey the [[cats.kernel.Hash]] instance used for hashing keys.
  * @return a new [[cats.data.HashMap]] which contains all elements of `kvs`.
  */
  final def apply[K, V](kvs: (K, V)*)(implicit hashKey: Hash[K]) =
    fromSeq(kvs)

  /**
  * Creates a new [[cats.data.HashMap]] which contains all elements of `seq`.
  *
  * @param seq the sequence of elements to add to the [[cats.data.HashMap]].
  * @param hashKey the [[cats.kernel.Hash]] instance used for hashing values.
  * @return a new [[cats.data.HashMap]] which contains all elements of `seq`.
  */
  final def fromSeq[K, V](seq: Seq[(K, V)])(implicit hashKey: Hash[K]): HashMap[K, V] = {
    val rootNode = seq.foldLeft(Node.empty[K, V]) { case (node, (k, v)) =>
      node.add(k, hashKey.hash(k), v, 0)
    }
    new HashMap(rootNode)
  }

  sealed abstract private[data] class Node[K, +V] {

    /**
      * @return The number of value and node elements in the contents array of this trie node.
      */
    def allElements: Int

    /**
      * @return The number of value elements in the contents array of this trie node.
      */
    def valueElements: Int

    /**
      * @return The number of node elements in the contents array of this trie node.
      */
    def nodeElements: Int

    /**
     * @return the number of value elements in this subtree.
     */
    def size: Int

    /**
      * @param index the index of the value among the value elements of this trie node.
      * @return the key element at the provided `index`.
      */
    def getKey(index: Int): K

    /**
      * @param index the index of the value among the value elements of this trie node.
      * @return the value at the provided `index`.
      */
    def getValue(index: Int): V

    /**
      * @param index the index of the value among the value elements of this trie node.
      * @return the key-value element at the provided `index`.
      */
    def getMapping(index: Int): (K, V)

    /**
      * @param index the index of the node among the node elements of this trie node.
      * @return the node element at the provided `index`.
      */
    def getNode(index: Int): Node[K, V]

    /**
      * @return a [[scala.Boolean]] indicating whether the current trie node contains any node elements.
      */
    def hasNodes: Boolean

    /**
      * @return a [[scala.Boolean]] indicating whether the current trie node contains any value elements.
      */
    def hasValues: Boolean

    /**
      * Apply f to each key-value pair of the current trie node and its sub-nodes for its side effects.
      *
      * @param f
      */
    def foreach[U](f: (K, V) => U): Unit

    /**
      * Determines whether the current trie node or its sub-nodes contain the provided key.
      *
      * @param key the key to query
      * @param keyHash the hash of the key to query
      * @param depth the 0-indexed depth in the trie structure.
      * @return a [[scala.Boolean]] indicating whether this [[HashMap.Node]] or any of its child nodes contains the element.
      */
    def contains(key: K, keyHash: Int, depth: Int): Boolean

    /**
      * Get the value associated with `key` in the current trie node or its sub-nodes.
      *
      * @param key the key to query
      * @param keyHash the hash of the key to query
      * @param depth the 0-indexed depth in the trie structure.
      * @return a [[scala.Some]] containing the value if present, else [[scala.None]].
      */
    def get(key: K, keyHash: Int, depth: Int): Option[V]

    /**
      * The current trie node updated to add the provided key-value pair.
      *
      * @param newKey the key to add.
      * @param newKeyHash the hash of the key to add.
      * @param value the value to add.
      * @param depth the 0-indexed depth in the trie structure.
      * @return a new [[HashMap.Node]] containing the element to add.
      */
    def add[VV >: V](newKey: K, newKeyHash: Int, value: VV, depth: Int): Node[K, VV]

    /**
      * The current trie node updated to remove the provided key.
      *
      * @param removeKey the key to remove.
      * @param removeKeyHash the hash of the element to remove.
      * @param depth the 0-indexed depth in the trie structure.
      * @return a new [[HashMap.Node]] with the element removed.
      */
    def remove(removeKey: K, removeKeyHash: Int, depth: Int): Node[K, V]

    /**
     * Typesafe equality operator.
     *
     * This method is similar to [[scala.Any#==]] except that it only allows two [[cats.data.HashMap.Node]]
     * values of the same key-value type to be compared to each other, and uses equality provided
     * by [[cats.kernel.Eq]] instances, rather than using the universal equality provided by
     * [[java.lang.Object#equals]].
     *
     * @param that the [[cats.data.HashMap.Node]] to check for equality with this node.
     * @param eqValue the [[cats.kernel.Eq]] instance to use for comparing values.
     * @return `true` if this node and `that` are equal, `false` otherwise.
     */
    def ===[VV >: V](that: Node[K, VV])(implicit eqValue: Eq[VV]): Boolean

    /**
      * An approximation of the CHAMP "branch size", used for the deletion algorithm.
      *
      * The branch size indicates the number of elements transitively reachable from this node, but that is expensive to compute.
      *
      * There are three important cases when implementing the deletion algorithm:
      * - a sub-tree has no elements ([[Node.SizeNone]])
      * - a sub-tree has exactly one element ([[Node.SizeOne]])
      * - a sub-tree has more than one element ([[Node.SizeMany]])
      *
      * This approximation assumes that nodes contain many elements (because the deletion algorithm inlines singleton nodes).
      *
      * @return either [[Node.SizeNone]], [[Node.SizeOne]] or [[Node.SizeMany]]
      */
    final def sizeHint = {
      if (nodeElements > 0)
        Node.SizeMany
      else
        (valueElements: @annotation.switch) match {
          case 0 => Node.SizeNone
          case 1 => Node.SizeOne
          case _ => Node.SizeMany
        }
    }
  }

  /**
    * A CHAMP hash collision node. In the event that the hash codes of multiple elements collide,
    * this node type is used to collect all of the colliding elements and implement the [[HashMap.Node]]
    * interface at a performance cost compared with a [[HashMap.BitMapNode]].
    *
    * @tparam A the type of the elements contained in this node.
    * @param collisionHash the hash value at which all of the contents of this node collide.
    * @param contents the value elements whose hashes collide.
    */
  final private[HashMap] class CollisionNode[K, +V](
    val collisionHash: Int,
    val contents: Vector[(K, V)]
  )(implicit hashKey: Hash[K])
      extends Node[K, V] {

    final def hasNodes: Boolean = false

    final def hasValues: Boolean = true

    final def allElements: Int = valueElements

    final def valueElements: Int = contents.size

    final def nodeElements: Int = 0

    final def size: Int = contents.size

    final def foreach[U](f: (K, V) => U): Unit =
      contents.foreach(f.tupled)

    final def contains(key: K, keyHash: Int, depth: Int): Boolean =
      collisionHash == keyHash && contents.exists { case (k, _) => hashKey.eqv(key, k) }

    final def get(key: K, keyHash: Int, depth: Int): Option[V] =
      if (collisionHash != keyHash) None
      else contents.collectFirst { case (k, v) if hashKey.eqv(key, k) => v }

    final def getKey(index: Int): K =
      contents(index)._1

    final def getValue(index: Int): V =
      contents(index)._2

    final def getMapping(index: Int): (K, V) =
      contents(index)

    final def getNode(index: Int): Node[K, V] =
      throw new IndexOutOfBoundsException("No sub-nodes present in hash-collision leaf node.")

    final def add[VV >: V](newKey: K, newKeyHash: Int, newValue: VV, depth: Int): Node[K, VV] =
      if (contains(newKey, newKeyHash, depth))
        this
      else
        new CollisionNode(newKeyHash, contents :+ (newKey -> newValue))

    final override def remove(key: K, keyHash: Int, depth: Int): Node[K, V] =
      if (!contains(key, keyHash, depth))
        this
      else {
        val newContents = contents.filterNot { case (k, _) => hashKey.eqv(key, k) }
        if (newContents.size > 1)
          new CollisionNode(collisionHash, newContents)
        else {
          // This is a singleton node so the depth doesn't matter;
          // we only need to index into it to inline the value in our parent node
          val mask = Node.maskFrom(collisionHash, 0)
          val bitPos = Node.bitPosFrom(mask)
          val newContentsArray = new Array[Any](Node.StrideLength * newContents.length)
          var i = 0
          while (i < newContents.length) {
            val (k, v) = newContents(i)
            val keyIndex = Node.StrideLength * i
            newContentsArray(keyIndex) = k
            newContentsArray(keyIndex + 1) = v
            i += 1
          }
          new BitMapNode[K, V](bitPos, 0, newContentsArray, newContents.size)
        }
      }

    final def ===[VV >: V](that: Node[K, VV])(implicit eqValue: Eq[VV]): Boolean = {
      (this eq that) || {
        that match {
          case node: CollisionNode[_, _] =>
            (this.collisionHash === node.collisionHash) &&
            (this.contents.size === node.contents.size) &&
            this.contents.forall { case (kl, vl) =>
              node.contents.exists { case (kr, vr) => hashKey.eqv(kl, kr) && eqValue.eqv(vl, vr) }
            }
          case _ =>
            false
        }
      }
    }

    final override def equals(that: Any): Boolean = that match {
      case node: CollisionNode[_, _] =>
        (this.collisionHash == node.collisionHash) &&
        (this.contents.size == node.contents.size) &&
        this.contents.forall(node.contents.contains)
      case _ =>
        false
    }

    final override def toString(): String = {
      s"""CollisionNode(hash=${collisionHash}, values=${contents.mkString("[", ",", "]")})"""
    }
  }

  /**
    * A CHAMP bitmap node. Stores value element and node element positions in the `contents` array
    * in the `valueMap` and `nodeMap` integer bitmaps.
    *
    * @tparam A the type of the elements contained in this node.
    * @param valueMap integer bitmap indicating the notional positions of value elements in the `contents` array.
    * @param nodeMap integer bitmap indicating the notional positions of node elements in the `contents` array.
    * @param contents an array of `A` value elements and `Node[A]` sub-node elements.
    * @param size the number of value elements in this subtree.
    */
  final private[HashMap] class BitMapNode[K, +V](
    val valueMap: Int,
    val nodeMap: Int,
    val contents: Array[Any],
    val size: Int
  )(implicit hashKey: Hash[K])
      extends Node[K, V] {

    final def hasValues: Boolean =
      valueMap != 0

    final def hasNodes: Boolean =
      nodeMap != 0

    final def allElements: Int =
      valueElements + nodeElements

    final def valueElements: Int =
      Integer.bitCount(valueMap)

    final def nodeElements: Int =
      Integer.bitCount(nodeMap)

    final private def hasNodeAt(bitPos: Int): Boolean =
      (nodeMap & bitPos) != 0

    final private def hasValueAt(bitPos: Int): Boolean =
      (valueMap & bitPos) != 0

    final def getKey(index: Int): K =
      contents(Node.StrideLength * index).asInstanceOf[K]

    final def getValue(index: Int): V =
      contents(Node.StrideLength * index + 1).asInstanceOf[V]

    final def getMapping(index: Int): (K, V) =
      contents(Node.StrideLength * index).asInstanceOf[K] ->
        contents(Node.StrideLength * index + 1).asInstanceOf[V]

    final def getNode(index: Int): Node[K, V] =
      contents(contents.length - 1 - index).asInstanceOf[Node[K, V]]

    final def foreach[U](f: (K, V) => U): Unit = {
      var i = 0
      while (i < valueElements) {
        f.tupled(getMapping(i))
        i += 1
      }

      i = 0
      while (i < nodeElements) {
        getNode(i).foreach(f)
        i += 1
      }
    }

    final def contains(key: K, keyHash: Int, depth: Int): Boolean = {
      val mask = Node.maskFrom(keyHash, depth)
      val bitPos = Node.bitPosFrom(mask)

      if (hasValueAt(bitPos)) {
        val index = Node.indexFrom(valueMap, bitPos)
        hashKey.eqv(key, getKey(index))
      } else if (hasNodeAt(bitPos)) {
        val index = Node.indexFrom(nodeMap, bitPos)
        getNode(index).contains(key, keyHash, depth + 1)
      } else {
        false
      }
    }

    final def get(key: K, keyHash: Int, depth: Int): Option[V] = {
      val mask = Node.maskFrom(keyHash, depth)
      val bitPos = Node.bitPosFrom(mask)

      if (hasValueAt(bitPos)) {
        val index = Node.indexFrom(valueMap, bitPos)
        if (hashKey.eqv(key, getKey(index))) {
          Some(getValue(index))
        } else {
          None
        }
      } else if (hasNodeAt(bitPos)) {
        val index = Node.indexFrom(nodeMap, bitPos)
        getNode(index).get(key, keyHash, depth + 1)
      } else {
        None
      }
    }

    final private def mergeValues[VV >: V](
      left: K,
      leftHash: Int,
      leftValue: VV,
      right: K,
      rightHash: Int,
      rightValue: VV,
      depth: Int
    ): Node[K, VV] = {
      if (depth >= Node.MaxDepth) {
        new CollisionNode[K, VV](leftHash, Vector(left -> leftValue, right -> rightValue))
      } else {
        val leftMask = Node.maskFrom(leftHash, depth)
        val rightMask = Node.maskFrom(rightHash, depth)
        if (leftMask != rightMask) {
          val valueMap = Node.bitPosFrom(leftMask) | Node.bitPosFrom(rightMask)
          if (leftMask < rightMask) {
            new BitMapNode[K, VV](valueMap, 0, Array(left, leftValue, right, rightValue), 2)
          } else {
            new BitMapNode[K, VV](valueMap, 0, Array(right, rightValue, left, leftValue), 2)
          }
        } else {
          val nodeMap = Node.bitPosFrom(leftMask)
          val node = mergeValues(left, leftHash, leftValue, right, rightHash, rightValue, depth + 1)
          new BitMapNode[K, VV](0, nodeMap, Array(node), node.size)
        }
      }
    }

    final private def mergeValuesIntoNode[VV >: V](
      bitPos: Int,
      left: K,
      leftHash: Int,
      leftValue: VV,
      right: K,
      rightHash: Int,
      rightValue: VV,
      depth: Int
    ): Node[K, VV] = {
      val newNode = mergeValues(left, leftHash, leftValue, right, rightHash, rightValue, depth)
      val valueIndex = Node.StrideLength * Node.indexFrom(valueMap, bitPos)
      val nodeIndex = contents.length - Node.StrideLength - Node.indexFrom(nodeMap, bitPos)
      val newContents = new Array[Any](contents.length - 1)

      System.arraycopy(contents, 0, newContents, 0, valueIndex)
      System.arraycopy(contents, valueIndex + Node.StrideLength, newContents, valueIndex, nodeIndex - valueIndex)

      newContents(nodeIndex) = newNode

      System.arraycopy(
        contents,
        nodeIndex + Node.StrideLength,
        newContents,
        nodeIndex + 1,
        contents.length - nodeIndex - Node.StrideLength
      )

      new BitMapNode[K, V](valueMap ^ bitPos, nodeMap | bitPos, newContents, size + 1)
    }

    final private def replaceNode[VV >: V](index: Int, oldNode: Node[K, VV], newNode: Node[K, VV]): Node[K, VV] = {
      val targetIndex = contents.length - 1 - index
      val newContents = new Array[Any](contents.length)
      System.arraycopy(contents, 0, newContents, 0, contents.length)
      newContents(targetIndex) = newNode
      new BitMapNode[K, V](valueMap, nodeMap, newContents, size + (newNode.size - oldNode.size))
    }

    final private def updateNode[VV >: V](
      bitPos: Int,
      newKey: K,
      newKeyHash: Int,
      newValue: VV,
      depth: Int
    ): Node[K, VV] = {
      val index = Node.indexFrom(nodeMap, bitPos)
      val subNode = getNode(index)
      val newSubNode = subNode.add(newKey, newKeyHash, newValue, depth + 1)

      if (newSubNode eq subNode)
        this
      else
        replaceNode(index, subNode, newSubNode)
    }

    final private def replaceValueAtIndex[VV >: V](index: Int, newValue: VV): Node[K, VV] = {
      val valueIndex = Node.StrideLength * index + 1
      val newContents = new Array[Any](contents.length)
      System.arraycopy(contents, 0, newContents, 0, contents.length)
      newContents(valueIndex) = newValue
      new BitMapNode[K, V](valueMap, nodeMap, newContents, size)
    }

    final private def updateKeyValue[VV >: V](
      bitPos: Int,
      newKey: K,
      newKeyHash: Int,
      newValue: VV,
      depth: Int
    ): Node[K, VV] = {
      val index = Node.indexFrom(valueMap, bitPos)
      val (existingKey, existingValue) = getMapping(index)
      if (hashKey.eqv(existingKey, newKey)) {
        replaceValueAtIndex(index, newValue)
      } else
        mergeValuesIntoNode(
          bitPos,
          existingKey,
          hashKey.hash(existingKey),
          existingValue,
          newKey,
          newKeyHash,
          newValue,
          depth + 1
        )
    }

    final private def appendKeyValue[VV >: V](bitPos: Int, newKey: K, newValue: VV): Node[K, VV] = {
      val index = Node.StrideLength * Node.indexFrom(valueMap, bitPos)
      val newContents = new Array[Any](contents.length + Node.StrideLength)
      System.arraycopy(contents, 0, newContents, 0, index)
      newContents(index) = newKey
      newContents(index + 1) = newValue
      System.arraycopy(contents, index, newContents, index + Node.StrideLength, contents.length - index)
      new BitMapNode[K, V](valueMap | bitPos, nodeMap, newContents, size + 1)
    }

    final def add[VV >: V](newKey: K, newKeyHash: Int, newValue: VV, depth: Int): Node[K, VV] = {
      val mask = Node.maskFrom(newKeyHash, depth)
      val bitPos = Node.bitPosFrom(mask)

      if (hasValueAt(bitPos)) {
        updateKeyValue(bitPos, newKey, newKeyHash, newValue, depth)
      } else if (hasNodeAt(bitPos)) {
        updateNode(bitPos, newKey, newKeyHash, newValue, depth)
      } else {
        appendKeyValue(bitPos, newKey, newValue)
      }
    }

    final private def removeKeyValue(bitPos: Int, removeKey: K, removeKeyHash: Int, depth: Int): Node[K, V] = {
      val index = Node.indexFrom(valueMap, bitPos)
      val existingKey = getKey(index)
      if (!hashKey.eqv(existingKey, removeKey)) {
        this
      } else if (allElements == 1) {
        Node.empty[K, V]
      } else {
        val keyIndex = Node.StrideLength * index
        val newContents = new Array[Any](contents.length - Node.StrideLength)

        // If this element will be propagated or inlined, calculate the new valueMap at depth - 1
        val newBitPos =
          if (valueElements == 2 && nodeElements == 0 && depth > 0)
            Node.bitPosFrom(Node.maskFrom(removeKeyHash, depth - 1))
          else
            valueMap ^ bitPos

        System.arraycopy(contents, 0, newContents, 0, keyIndex)

        System.arraycopy(
          contents,
          keyIndex + Node.StrideLength,
          newContents,
          keyIndex,
          contents.length - keyIndex - Node.StrideLength
        )

        new BitMapNode[K, V](newBitPos, nodeMap, newContents, size - 1)
      }
    }

    final private def inlineSubNodeKeyValue[VV >: V](bitPos: Int, newSubNode: Node[K, VV]): Node[K, VV] = {
      val nodeIndex = contents.length - 1 - Node.indexFrom(nodeMap, bitPos)
      val keyIndex = Node.StrideLength * Node.indexFrom(valueMap, bitPos)
      val newContents = new Array[Any](contents.length + 1)
      val (key, value) = newSubNode.getMapping(0)

      System.arraycopy(contents, 0, newContents, 0, keyIndex)

      newContents(keyIndex) = key
      newContents(keyIndex + 1) = value

      System.arraycopy(contents, keyIndex, newContents, keyIndex + Node.StrideLength, nodeIndex - keyIndex)

      System.arraycopy(
        contents,
        nodeIndex + 1,
        newContents,
        nodeIndex + Node.StrideLength,
        contents.length - nodeIndex - 1
      )

      new BitMapNode[K, V](valueMap | bitPos, nodeMap ^ bitPos, newContents, size - 1)
    }

    final private def removeKeyValueFromSubNode(
      bitPos: Int,
      removeKey: K,
      removeKeyHash: Int,
      depth: Int
    ): Node[K, V] = {
      val index = Node.indexFrom(nodeMap, bitPos)
      val subNode = getNode(index)
      val newSubNode = subNode.remove(removeKey, removeKeyHash, depth + 1)

      if (newSubNode eq subNode)
        this
      else if (valueElements == 0 && nodeElements == 1) {
        if (newSubNode.sizeHint == Node.SizeOne) {
          newSubNode
        } else {
          replaceNode(index, subNode, newSubNode)
        }
      } else if (newSubNode.sizeHint == Node.SizeOne) {
        inlineSubNodeKeyValue(bitPos, newSubNode)
      } else {
        replaceNode(index, subNode, newSubNode)
      }
    }

    final override def remove(removeKey: K, removeKeyHash: Int, depth: Int): Node[K, V] = {
      val mask = Node.maskFrom(removeKeyHash, depth)
      val bitPos = Node.bitPosFrom(mask)

      if (hasValueAt(bitPos)) {
        removeKeyValue(bitPos, removeKey, removeKeyHash, depth)
      } else if (hasNodeAt(bitPos)) {
        removeKeyValueFromSubNode(bitPos, removeKey, removeKeyHash, depth)
      } else {
        this
      }
    }

    final override def ===[VV >: V](that: Node[K, VV])(implicit eqValue: Eq[VV]): Boolean = {
      (this eq that) || {
        that match {
          case node: BitMapNode[_, _] =>
            (this.valueMap === node.valueMap) &&
            (this.nodeMap === node.nodeMap) &&
            (this.size === node.size) && {
              var i = 0
              while (i < valueElements) {
                val (kl, vl) = getMapping(i)
                val (kr, vr) = node.getMapping(i)
                if (hashKey.neqv(kl, kr) || eqValue.neqv(vl, vr)) return false
                i += 1
              }
              i = 0
              while (i < nodeElements) {
                if (!(getNode(i).===[VV](node.getNode(i)))) return false
                i += 1
              }
              true
            }
          case _ =>
            false
        }
      }
    }

    final override def equals(that: Any): Boolean = that match {
      case node: BitMapNode[_, _] =>
        (this eq node) || {
          (this.valueMap == node.valueMap) &&
          (this.nodeMap == node.nodeMap) &&
          (this.size == node.size) &&
          Arrays.equals(
            this.contents.asInstanceOf[Array[Object]],
            node.contents.asInstanceOf[Array[Object]]
          )
        }
      case _ =>
        false
    }

    final override def toString(): String = {
      val valueMapStr =
        ("0" * Integer.numberOfLeadingZeros(if (valueMap != 0) valueMap else 1)) + Integer.toBinaryString(valueMap)
      val nodeMapStr =
        ("0" * Integer.numberOfLeadingZeros(if (nodeMap != 0) nodeMap else 1)) + Integer.toBinaryString(nodeMap)
      val contentsStr =
        contents.mkString("[", ", ", "]")

      s"""BitMapNode(valueMap=$valueMapStr, nodeMap=$nodeMapStr, size=$size, contents=${contentsStr})"""
    }
  }

  private[HashMap] object Node {
    final val StrideLength = 2
    final val BitPartitionSize = 5
    final val BitPartitionMask = (1 << BitPartitionSize) - 1
    final val MaxDepth = 7

    final val SizeNone = 0
    final val SizeOne = 1
    final val SizeMany = 2

    /**
      * The `mask` is a 5-bit segment of a 32-bit element hash.
      *
      * The `depth` value is used to determine which segment of the hash we are currently inspecting by shifting the `elementHash` to the right in [[Node.BitPartitionSize]] bit increments.
      *
      * A 5-bit segment of the hash can represent numbers 0 to 31, which matches the branching factor of the trie structure.
      *
      * It represents the notional index of the element in the current trie node.
      *
      * @param elementHash the hash of the element we are operating on.
      * @param depth the depth of the current node in the trie structure.
      * @return the relevant 5-bit segment of the `elementHash`.
      */
    final def maskFrom(elementHash: Int, depth: Int): Int =
      (elementHash >>> (depth * Node.BitPartitionSize)) & BitPartitionMask

    /**
      * Sets a single bit at the position of the notional index indicated by `mask`.
      *
      * Used to determine the bit which represents the notional index of a data value or node in the trie node bitmaps.
      *
      * @param mask the notional index of an element at this depth in the trie.
      * @return an integer with a single bit set at the notional index indicated by `mask`.
      */
    final def bitPosFrom(mask: Int): Int =
      1 << mask

    /**
      * Calculates the absolute index of an element in the contents array of a trie node.
      *
      * This is calculated by counting how many bits are set to the right of the notional index in the relevant bitmap.
      *
      * @param bitMap the bitmap indicating either data value or node positions in the contents array.
      * @param bitPos the notional index of the element in the trie node.
      * @return the absolute index of an element in the contents array.
      */
    final def indexFrom(bitMap: Int, bitPos: Int): Int =
      Integer.bitCount(bitMap & (bitPos - 1))

    /**
      * Creates a new empty bitmap node.
      *
      * @param hash the [[cats.kernel.Hash]] instance to use to hash elements.
      * @return a new empty bitmap node.
      */
    final def empty[K, V](implicit hashKey: Hash[K]): Node[K, V] =
      new BitMapNode[K, V](0, 0, Array.empty[Any], 0)
  }

  private[HashMap] class Iterator[K, V] extends scala.collection.AbstractIterator[(K, V)] {
    private var currentNode: Node[K, V] = null

    private var currentValuesIndex: Int = 0
    private var currentValuesLength: Int = 0

    private var currentDepth: Int = -1

    private val nodeStack: Array[Node[K, V]] =
      new Array(Node.MaxDepth)

    private val nodeIndicesAndLengths: Array[Int] =
      new Array(Node.MaxDepth * 2)

    def this(rootNode: Node[K, V]) = {
      this()
      if (rootNode.hasNodes) pushNode(rootNode)
      if (rootNode.hasValues) pushValues(rootNode)
    }

    final private def pushNode(node: Node[K, V]): Unit = {
      currentDepth += 1

      val cursorIndex = currentDepth * 2
      val lengthIndex = currentDepth * 2 + 1

      nodeStack(currentDepth) = node

      nodeIndicesAndLengths(cursorIndex) = 0
      nodeIndicesAndLengths(lengthIndex) = node.nodeElements
    }

    final private def pushValues(node: Node[K, V]): Unit = {
      currentNode = node
      currentValuesIndex = 0
      currentValuesLength = node.valueElements
    }

    final private def getMoreValues(): Boolean = {
      var foundMoreValues = false

      while (!foundMoreValues && currentDepth >= 0) {
        val cursorIndex = currentDepth * 2
        val lengthIndex = currentDepth * 2 + 1

        val nodeIndex = nodeIndicesAndLengths(cursorIndex)
        val nodeLength = nodeIndicesAndLengths(lengthIndex)

        if (nodeIndex < nodeLength) {
          val nextNode = nodeStack(currentDepth)
            .getNode(nodeIndex)

          if (nextNode.hasNodes) {
            pushNode(nextNode)
          }

          if (nextNode.hasValues) {
            pushValues(nextNode)
            foundMoreValues = true
          }

          nodeIndicesAndLengths(cursorIndex) += 1

        } else {
          currentDepth -= 1
        }
      }

      foundMoreValues
    }

    final override def hasNext: Boolean =
      (currentValuesIndex < currentValuesLength) || getMoreValues()

    final override def next(): (K, V) = {
      if (!hasNext) throw new NoSuchElementException
      val value = currentNode.getMapping(currentValuesIndex)
      currentValuesIndex += 1
      value
    }
  }

  private[HashMap] class ReverseIterator[K, V] extends scala.collection.AbstractIterator[(K, V)] {
    private var currentNode: Node[K, V] = null

    private var currentValuesIndex: Int = -1

    private var currentDepth: Int = -1

    private val nodeStack: Array[Node[K, V]] =
      new Array(Node.MaxDepth + 1)

    private val nodeIndices: Array[Int] =
      new Array(Node.MaxDepth + 1)

    def this(rootNode: Node[K, V]) = {
      this()
      pushNode(rootNode)
      getMoreValues()
    }

    final private def pushNode(node: Node[K, V]): Unit = {
      currentDepth += 1
      nodeStack(currentDepth) = node
      nodeIndices(currentDepth) = node.nodeElements - 1
    }

    final private def pushValues(node: Node[K, V]): Unit = {
      currentNode = node
      currentValuesIndex = node.valueElements - 1
    }

    final private def getMoreValues(): Boolean = {
      var foundMoreValues = false

      while (!foundMoreValues && currentDepth >= 0) {
        val nodeIndex = nodeIndices(currentDepth)
        nodeIndices(currentDepth) -= 1

        if (nodeIndex >= 0) {
          pushNode(nodeStack(currentDepth).getNode(nodeIndex))
        } else {
          val currentNode = nodeStack(currentDepth)
          currentDepth -= 1
          if (currentNode.hasValues) {
            pushValues(currentNode)
            foundMoreValues = true
          }
        }
      }

      foundMoreValues
    }

    final override def hasNext: Boolean =
      (currentValuesIndex >= 0) || getMoreValues()

    final override def next(): (K, V) = {
      if (!hasNext) throw new NoSuchElementException
      val value = currentNode.getMapping(currentValuesIndex)
      currentValuesIndex -= 1
      value
    }
  }

}

sealed abstract private[data] class HashMapInstances extends HashMapInstances1 {
  implicit def catsDataUnorderedTraverseForHashMap[K: Hash]: UnorderedTraverse[HashMap[K, *]] =
    new UnorderedTraverse[HashMap[K, *]] {
      def unorderedFoldMap[U, V](hm: HashMap[K, U])(f: U => V)(implicit V: CommutativeMonoid[V]): V =
        V.combineAll(hm.iterator.map { case (_, u) => f(u) })

      def unorderedTraverse[G[_], U, V](hashMap: HashMap[K, U])(f: U => G[V])(implicit
        G: CommutativeApplicative[G]
      ): G[HashMap[K, V]] = {
        val emptyHm: Eval[G[HashMap[K, V]]] =
          Always(G.pure(HashMap.empty[K, V]))

        val gHashMap = Foldable
          .iterateRight(Eval.always(hashMap.iterator), emptyHm) { case ((k, u), hm) =>
            G.map2Eval(f(u), hm) { (v, map) =>
              map.add(k, v)
            }
          }

        gHashMap.value
      }
    }

  implicit def catsDataCommutativeMonoidForHashMap[K: Hash, V: CommutativeSemigroup]: CommutativeMonoid[HashMap[K, V]] =
    new HashMapMonoid[K, V] with CommutativeMonoid[HashMap[K, V]]

  implicit def catsDataShowForHashMap[K: Show, V: Show]: Show[HashMap[K, V]] =
    Show.show[HashMap[K, V]](_.show)

  implicit def catsDataHashForHashMap[K, V: Hash]: Hash[HashMap[K, V]] =
    new Hash[HashMap[K, V]] {
      def hash(hm: HashMap[K, V]): Int = hm.hash
      def eqv(x: HashMap[K, V], y: HashMap[K, V]): Boolean = x === y
    }
}

sealed abstract private[data] class HashMapInstances1 {
  implicit def catsDataMonoidForHashMap[K: Hash, V: Semigroup]: Monoid[HashMap[K, V]] =
    new HashMapMonoid[K, V]
}

class HashMapMonoid[K: Hash, V](implicit V: Semigroup[V]) extends Monoid[HashMap[K, V]] {

  def empty: HashMap[K, V] = HashMap.empty[K, V]

  def combine(xs: HashMap[K, V], ys: HashMap[K, V]): HashMap[K, V] = {
    if (xs.size <= ys.size) {
      xs.iterator.foldLeft(ys) { case (my, (k, x)) =>
        my.add(k, Semigroup.maybeCombine(x, my.get(k)))
      }
    } else {
      ys.iterator.foldLeft(xs) { case (mx, (k, y)) =>
        mx.add(k, Semigroup.maybeCombine(mx.get(k), y))
      }
    }
  }
}
