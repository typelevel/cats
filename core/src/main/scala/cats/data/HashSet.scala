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

/* This file is derived from https://github.com/scala/scala/blob/v2.13.8/src/library/scala/collection/immutable/HashSet.scala,
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

package cats.data

import cats.Show
import cats.UnorderedFoldable
import cats.kernel.CommutativeMonoid
import cats.kernel.Hash
import cats.kernel.instances.StaticMethods
import cats.syntax.eq._
import java.util.Arrays

import HashSet.improve

/**
  * An immutable hash set using [[cats.kernel.Hash]] for hashing.
  *
  * Implemented using the CHAMP encoding.
  *
  * @see [[https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf Efficient Immutable Collections]]
  *
  * @tparam A the type of the elements contained in this hash set.
  * @param hash the [[cats.kernel.Hash]] instance used for hashing values.
  */
final class HashSet[A] private (private val rootNode: HashSet.Node[A])(implicit hash: Hash[A])
    extends HashSetCompat[A] {

  /**
    * An iterator for this set that can be used only once.
    *
    * @return an iterator that iterates through the elements of this set.
    */
  final def iterator: Iterator[A] =
    new HashSet.Iterator(rootNode)

  /**
    * A reverse iterator for this set that can be used only once.
    *
    * @return an iterator that iterates through this set in the reverse order of [[HashSet#iterator]].
    */
  final def reverseIterator: Iterator[A] =
    new HashSet.ReverseIterator(rootNode)

  /**
    * The size of this set.
    *
    * @return the number of elements in this set.
    */
  final def size: Int = rootNode.size

  /**
    * Tests whether the set is empty.
    *
    * @return `true` if the set contains no elements, `false` otherwise.
    */
  final def isEmpty: Boolean = size == 0

  /**
    * Tests whether the set is not empty.
    *
    * @return `true` if the set contains at least one element, `false` otherwise.
    */
  final def nonEmpty: Boolean = !isEmpty

  /**
    * Apply `f` to each element for its side effects.
    *
    * @param f the function to apply to each element.
    */
  final def foreach[U](f: A => U): Unit =
    rootNode.foreach(f)

  /**
    * Test whether the set contains `value`.
    *
    * @param value the element to check for set membership.
    * @return `true` if the set contains `value`, `false` otherwise.
    */
  final def contains(value: A): Boolean =
    rootNode.contains(value, improve(hash.hash(value)), 0)

  /**
    * Creates a new set with an additional element, unless the element is already present.
    *
    * @param value the element to be added.
    * @return a new set that contains all elements of this set and that also contains `value`.
    */
  final def add(value: A): HashSet[A] = {
    val valueHash = improve(hash.hash(value))
    val newRootNode = rootNode.add(value, valueHash, 0)

    if (newRootNode eq rootNode)
      this
    else
      new HashSet(newRootNode)
  }

  /**
    * Creates a new set with the given element removed from this set.
    *
    * @param value the element to be removed.
    * @return a new set that contains all elements of this set but that does not contain `value`.
    */
  final def remove(value: A): HashSet[A] = {
    val valueHash = improve(hash.hash(value))
    val newRootNode = rootNode.remove(value, valueHash, 0)

    if (newRootNode eq rootNode)
      this
    else
      new HashSet(newRootNode)
  }

  /**
    * Creates a new set with all of the elements of both this set and the provided `set`.
    *
    * @param set the set whose values should be added to this set.
    * @return a new set that contains all elements of this set and all of the elements of `set`.
    */
  final def union(set: HashSet[A]): HashSet[A] = {
    if (this.isEmpty)
      set
    else if (set.isEmpty)
      this
    else {
      val newRootNode = set.iterator.foldLeft(rootNode) { case (node, a) =>
        node.add(a, improve(hash.hash(a)), 0)
      }

      if (newRootNode eq rootNode)
        this
      else
        new HashSet(newRootNode)
    }
  }

  /**
   * Typesafe equality operator.
   *
   * This method is similar to [[scala.Any#==]] except that it only allows two [[cats.data.HashSet]]
   * values of the same element type to be compared to each other, and uses equality provided
   * by [[cats.kernel.Eq]] instances, rather than using the universal equality provided by
   * [[java.lang.Object#equals]].
   *
   * @param that the [[cats.data.HashSet]] to check for equality with this set.
   * @return `true` if this set and `that` are equal, `false` otherwise.
   */
  final def ===(that: HashSet[A]): Boolean = {
    (this eq that) || (this.rootNode === that.rootNode)
  }

  final override def equals(that: Any): Boolean = that match {
    case set: HashSet[_] =>
      (this eq set) || (this.rootNode == set.rootNode)
    case _ =>
      false
  }

  final override def hashCode(): Int =
    StaticMethods.unorderedHash(this.iterator)(this.hash)

  /**
   * Typesafe stringification operator.
   *
   * This method is similar to [[java.lang.Object#toString]] except that it stringifies values according
   * to [[cats.Show]] instances.
   *
   * @param show the [[cats.Show]] instance to use for showing values of type `A`.
   * @return a [[java.lang.String]] representation of this set.
   */
  final def show(implicit show: Show[A]): String =
    iterator.map(show.show).mkString("HashSet(", ", ", ")")

  final override def toString() =
    iterator.mkString("HashSet(", ", ", ")")
}

object HashSet {
  final private[HashSet] def improve(hash: Int): Int =
    scala.util.hashing.byteswap32(hash)

  /**
    * Creates a new empty [[cats.data.HashSet]] which uses `hash` for hashing.
    *
    * @param hash the [[cats.kernel.Hash]] instance used for hashing values.
    * @return a new empty [[cats.data.HashSet]].
    */
  final def empty[A](implicit hash: Hash[A]): HashSet[A] =
    new HashSet(Node.empty[A])

  /**
  * Creates a new [[cats.data.HashSet]] which contains all elements of `as`.
  *
  * @param as the elements to add to the [[cats.data.HashSet]].
  * @param hash the [[cats.kernel.Hash]] instance used for hashing values.
  * @return a new [[cats.data.HashSet]] which contains all elements of `as`.
  */
  final def apply[A](as: A*)(implicit hash: Hash[A]) =
    fromSeq(as)

  /**
  * Creates a new [[cats.data.HashSet]] which contains all elements of `seq`.
  *
  * @param seq the sequence of elements to add to the [[cats.data.HashSet]].
  * @param hash the [[cats.kernel.Hash]] instance used for hashing values.
  * @return a new [[cats.data.HashSet]] which contains all elements of `seq`.
  */
  final def fromSeq[A](seq: Seq[A])(implicit hash: Hash[A]): HashSet[A] = {
    val rootNode = seq.foldLeft(Node.empty[A]) { case (node, a) =>
      node.add(a, improve(hash.hash(a)), 0)
    }
    new HashSet(rootNode)
  }

  sealed abstract private class Node[A] {

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
      * @return the value element at the provided `index`.
      */
    def getValue(index: Int): A

    /**
      * @param index the index of the node among the node elements of this trie node.
      * @return the node element at the provided `index`.
      */
    def getNode(index: Int): Node[A]

    /**
      * @return a [[scala.Boolean]] indicating whether the current trie node contains any node elements.
      */
    def hasNodes: Boolean

    /**
      * @return a [[scala.Boolean]] indicating whether the current trie node contains any value elements.
      */
    def hasValues: Boolean

    /**
      * Apply f to each element of the current trie node and its sub-nodes for its side effects.
      *
      * @param f
      */
    def foreach[U](f: A => U): Unit

    /**
      * Determines whether the current trie node or its sub-nodes contain the provided element.
      *
      * @param element the element to query
      * @param elementHash the hash of the element to query
      * @param depth the 0-indexed depth in the trie structure.
      * @return a [[scala.Boolean]] indicating whether this [[HashSet.Node]] or any of its child nodes contains the element.
      */
    def contains(element: A, elementHash: Int, depth: Int): Boolean

    /**
      * The current trie node updated to add the provided element.
      *
      * @param newElement the element to add.
      * @param newElementHash the hash of the element to add.
      * @param depth the 0-indexed depth in the trie structure.
      * @return a new [[HashSet.Node]] containing the element to add.
      */
    def add(newElement: A, newElementHash: Int, depth: Int): Node[A]

    /**
      * The current trie node updated to remove the provided element.
      *
      * @param removeElement the element to remove.
      * @param removeElementHash the 32-bit hash of the element to remove.
      * @param depth the 0-indexed depth in the trie structure.
      * @return a new [[HashSet.Node]] with the element removed.
      */
    def remove(removeElement: A, removeElementHash: Int, depth: Int): Node[A]

    /**
     * Typesafe equality operator.
     *
     * This method is similar to [[scala.Any#==]] except that it only allows two [[cats.data.HashSet.Node]]
     * values of the same element type to be compared to each other, and uses equality provided
     * by [[cats.kernel.Eq]] instances, rather than using the universal equality provided by
     * [[java.lang.Object#equals]].
     *
     * @param that the [[cats.data.HashSet.Node]] to check for equality with this set.
     * @return `true` if this set and `that` are equal, `false` otherwise.
     */
    def ===(that: Node[A]): Boolean

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
    * A CHAMP hash collision node. In the event that the 32-bit hash codes of multiple elements collide,
    * this node type is used to collect all of the colliding elements and implement the [[HashSet.Node]]
    * interface at a performance cost compared with a [[HashSet.BitMapNode]].
    *
    * @tparam A the type of the elements contained in this node.
    * @param collisionHash the hash value at which all of the contents of this node collide.
    * @param contents the value elements whose hashes collide.
    */
  final private class CollisionNode[A](
    val collisionHash: Int,
    val contents: Vector[A]
  )(implicit hash: Hash[A])
      extends Node[A] {

    final def hasNodes: Boolean = false

    final def hasValues: Boolean = true

    final def allElements: Int = valueElements

    final def valueElements: Int = contents.size

    final def nodeElements: Int = 0

    final def size: Int = contents.size

    final def foreach[U](f: A => U): Unit =
      contents.foreach(f)

    final def contains(element: A, elementHash: Int, depth: Int): Boolean =
      collisionHash == elementHash && contents.exists(hash.eqv(element, _))

    final def getValue(index: Int): A =
      contents(index)

    final def getNode(index: Int): Node[A] =
      throw new IndexOutOfBoundsException("No sub-nodes present in hash-collision leaf node.")

    final def add(newElement: A, newElementHash: Int, depth: Int): Node[A] =
      if (contains(newElement, newElementHash, depth))
        this
      else
        new CollisionNode[A](newElementHash, contents :+ newElement)

    final override def remove(element: A, elementHash: Int, depth: Int): Node[A] =
      if (!contains(element, elementHash, depth))
        this
      else {
        val newContents = contents.filterNot(hash.eqv(element, _))
        if (newContents.size > 1)
          new CollisionNode(collisionHash, newContents)
        else {
          // This is a singleton node so the depth doesn't matter;
          // we only need to index into it to inline the value in our parent node
          val mask = Node.maskFrom(collisionHash, 0)
          val bitPos = Node.bitPosFrom(mask)
          new BitMapNode(bitPos, 0, newContents.toArray, newContents.size)
        }
      }

    final def ===(that: Node[A]): Boolean = {
      (this eq that) || {
        that match {
          case node: CollisionNode[_] =>
            (this.collisionHash === node.collisionHash) &&
            (this.contents.size === node.contents.size) &&
            this.contents.forall(a => node.contents.exists(hash.eqv(a, _)))
          case _ =>
            false
        }
      }
    }

    final override def equals(that: Any): Boolean = that match {
      case node: CollisionNode[_] =>
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
  final private class BitMapNode[A](
    val valueMap: Int,
    val nodeMap: Int,
    val contents: Array[Any],
    val size: Int
  )(implicit hash: Hash[A])
      extends Node[A] {

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

    final def getValue(index: Int): A =
      contents(index).asInstanceOf[A]

    final def getNode(index: Int): Node[A] =
      contents(contents.length - 1 - index).asInstanceOf[Node[A]]

    final def foreach[U](f: A => U): Unit = {
      var i = 0
      while (i < valueElements) {
        f(getValue(i))
        i += 1
      }

      i = 0
      while (i < nodeElements) {
        getNode(i).foreach(f)
        i += 1
      }
    }

    final private def mergeValues(left: A, leftHash: Int, right: A, rightHash: Int, depth: Int): Node[A] = {
      if (depth >= Node.MaxDepth) {
        new CollisionNode[A](leftHash, Vector(left, right))
      } else {
        val leftMask = Node.maskFrom(leftHash, depth)
        val rightMask = Node.maskFrom(rightHash, depth)
        if (leftMask != rightMask) {
          val valueMap = Node.bitPosFrom(leftMask) | Node.bitPosFrom(rightMask)
          if (leftMask < rightMask) {
            new BitMapNode(valueMap, 0, Array(left, right), 2)
          } else {
            new BitMapNode(valueMap, 0, Array(right, left), 2)
          }
        } else {
          val nodeMap = Node.bitPosFrom(leftMask)
          val node = mergeValues(left, leftHash, right, rightHash, depth + 1)
          new BitMapNode(0, nodeMap, Array(node), node.size)
        }
      }
    }

    final private def mergeValuesIntoNode(
      bitPos: Int,
      left: A,
      leftHash: Int,
      right: A,
      rightHash: Int,
      depth: Int
    ): Node[A] = {
      val newNode = mergeValues(left, leftHash, right, rightHash, depth)
      val valueIndex = Node.indexFrom(valueMap, bitPos)
      val nodeIndex = contents.length - 1 - Node.indexFrom(nodeMap, bitPos)
      val newContents = new Array[Any](contents.length)
      System.arraycopy(contents, 0, newContents, 0, valueIndex)
      System.arraycopy(contents, valueIndex + 1, newContents, valueIndex, nodeIndex - valueIndex)
      newContents(nodeIndex) = newNode
      System.arraycopy(contents, nodeIndex + 1, newContents, nodeIndex + 1, contents.length - nodeIndex - 1)
      new BitMapNode(valueMap ^ bitPos, nodeMap | bitPos, newContents, size + 1)
    }

    final private def replaceNode(index: Int, oldNode: Node[A], newNode: Node[A]): Node[A] = {
      val targetIndex = contents.length - 1 - index
      val newContents = new Array[Any](contents.length)
      System.arraycopy(contents, 0, newContents, 0, contents.length)
      newContents(targetIndex) = newNode
      new BitMapNode(valueMap, nodeMap, newContents, size + (newNode.size - oldNode.size))
    }

    final private def updateNode(bitPos: Int, newElement: A, newElementHash: Int, depth: Int): Node[A] = {
      val index = Node.indexFrom(nodeMap, bitPos)
      val subNode = getNode(index)
      val newSubNode = subNode.add(newElement, newElementHash, depth + 1)

      if (newSubNode eq subNode)
        this
      else
        replaceNode(index, subNode, newSubNode)
    }

    final private def updateValue(bitPos: Int, newElement: A, newElementHash: Int, depth: Int): Node[A] = {
      val index = Node.indexFrom(valueMap, bitPos)
      val existingElement = getValue(index)
      if (hash.eqv(existingElement, newElement))
        this
      else
        mergeValuesIntoNode(
          bitPos,
          existingElement,
          improve(hash.hash(existingElement)),
          newElement,
          newElementHash,
          depth + 1
        )
    }

    final private def appendValue(bitPos: Int, newElement: A): Node[A] = {
      val index = Node.indexFrom(valueMap, bitPos)
      val newContents = new Array[Any](contents.length + 1)
      System.arraycopy(contents, 0, newContents, 0, index)
      newContents(index) = newElement
      System.arraycopy(contents, index, newContents, index + 1, contents.length - index)
      new BitMapNode(valueMap | bitPos, nodeMap, newContents, size + 1)
    }

    final def contains(element: A, elementHash: Int, depth: Int): Boolean = {
      val mask = Node.maskFrom(elementHash, depth)
      val bitPos = Node.bitPosFrom(mask)

      if (hasValueAt(bitPos)) {
        val index = Node.indexFrom(valueMap, bitPos)
        hash.eqv(element, getValue(index))
      } else if (hasNodeAt(bitPos)) {
        val index = Node.indexFrom(nodeMap, bitPos)
        getNode(index).contains(element, elementHash, depth + 1)
      } else {
        false
      }
    }

    final def add(newElement: A, newElementHash: Int, depth: Int): Node[A] = {
      val mask = Node.maskFrom(newElementHash, depth)
      val bitPos = Node.bitPosFrom(mask)

      if (hasValueAt(bitPos)) {
        updateValue(bitPos, newElement, newElementHash, depth)
      } else if (hasNodeAt(bitPos)) {
        updateNode(bitPos, newElement, newElementHash, depth)
      } else {
        appendValue(bitPos, newElement)
      }
    }

    final private def removeValue(bitPos: Int, removeElement: A, removeElementHash: Int, depth: Int): Node[A] = {
      val index = Node.indexFrom(valueMap, bitPos)
      val existingElement = getValue(index)
      if (!hash.eqv(existingElement, removeElement)) {
        this
      } else if (allElements == 1) {
        Node.empty
      } else {
        val newContents = new Array[Any](contents.length - 1)

        // If this element will be propagated or inlined, calculate the new valueMap at depth - 1
        val newBitPos =
          if (valueElements == 2 && nodeElements == 0 && depth > 0)
            Node.bitPosFrom(Node.maskFrom(removeElementHash, depth - 1))
          else
            valueMap ^ bitPos

        System.arraycopy(contents, 0, newContents, 0, index)
        System.arraycopy(contents, index + 1, newContents, index, contents.length - index - 1)

        new BitMapNode(newBitPos, nodeMap, newContents, size - 1)
      }
    }

    final private def inlineSubNodeValue(bitPos: Int, newSubNode: Node[A]): Node[A] = {
      val nodeIndex = contents.length - 1 - Node.indexFrom(nodeMap, bitPos)
      val valueIndex = Node.indexFrom(valueMap, bitPos)
      val newContents = new Array[Any](contents.length)
      val value = newSubNode.getValue(0)
      System.arraycopy(contents, 0, newContents, 0, valueIndex)
      newContents(valueIndex) = value
      System.arraycopy(contents, valueIndex, newContents, valueIndex + 1, nodeIndex - valueIndex)
      System.arraycopy(contents, nodeIndex + 1, newContents, nodeIndex + 1, contents.length - nodeIndex - 1)
      new BitMapNode(valueMap | bitPos, nodeMap ^ bitPos, newContents, size - 1)
    }

    final private def removeValueFromSubNode(
      bitPos: Int,
      removeElement: A,
      removeElementHash: Int,
      depth: Int
    ): Node[A] = {
      val index = Node.indexFrom(nodeMap, bitPos)
      val subNode = getNode(index)
      val newSubNode = subNode.remove(removeElement, removeElementHash, depth + 1)

      if (newSubNode eq subNode)
        this
      else if (valueElements == 0 && nodeElements == 1) {
        if (newSubNode.sizeHint == Node.SizeOne) {
          newSubNode
        } else {
          replaceNode(index, subNode, newSubNode)
        }
      } else if (newSubNode.sizeHint == Node.SizeOne) {
        inlineSubNodeValue(bitPos, newSubNode)
      } else {
        replaceNode(index, subNode, newSubNode)
      }
    }

    final override def remove(removeElement: A, removeElementHash: Int, depth: Int): Node[A] = {
      val mask = Node.maskFrom(removeElementHash, depth)
      val bitPos = Node.bitPosFrom(mask)

      if (hasValueAt(bitPos)) {
        removeValue(bitPos, removeElement, removeElementHash, depth)
      } else if (hasNodeAt(bitPos)) {
        removeValueFromSubNode(bitPos, removeElement, removeElementHash, depth)
      } else {
        this
      }
    }

    final override def ===(that: Node[A]): Boolean = {
      (this eq that) || {
        that match {
          case node: BitMapNode[_] =>
            (this.valueMap === node.valueMap) &&
            (this.nodeMap === node.nodeMap) &&
            (this.size === node.size) && {
              var i = 0
              while (i < valueElements) {
                if (hash.neqv(getValue(i), node.getValue(i))) return false
                i += 1
              }
              i = 0
              while (i < nodeElements) {
                if (!(getNode(i) === node.getNode(i))) return false
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
      case node: BitMapNode[_] =>
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

  private[HashSet] object Node {
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
    final def empty[A](implicit hash: Hash[A]): Node[A] =
      new BitMapNode(0, 0, Array.empty[Any], 0)
  }

  private[HashSet] class Iterator[A] extends scala.collection.AbstractIterator[A] {
    private var currentNode: Node[A] = null

    private var currentValuesIndex: Int = 0
    private var currentValuesLength: Int = 0

    private var currentDepth: Int = -1

    private val nodeStack: Array[Node[A]] =
      new Array(Node.MaxDepth)

    private val nodeIndicesAndLengths: Array[Int] =
      new Array(Node.MaxDepth * 2)

    def this(rootNode: Node[A]) = {
      this()
      if (rootNode.hasNodes) pushNode(rootNode)
      if (rootNode.hasValues) pushValues(rootNode)
    }

    final private def pushNode(node: Node[A]): Unit = {
      currentDepth += 1

      val cursorIndex = currentDepth * 2
      val lengthIndex = currentDepth * 2 + 1

      nodeStack(currentDepth) = node

      nodeIndicesAndLengths(cursorIndex) = 0
      nodeIndicesAndLengths(lengthIndex) = node.nodeElements
    }

    final private def pushValues(node: Node[A]): Unit = {
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

    final override def next(): A = {
      if (!hasNext) throw new NoSuchElementException
      val value = currentNode.getValue(currentValuesIndex)
      currentValuesIndex += 1
      value
    }
  }

  private[HashSet] class ReverseIterator[A] extends scala.collection.AbstractIterator[A] {
    private var currentNode: Node[A] = null

    private var currentValuesIndex: Int = -1

    private var currentDepth: Int = -1

    private val nodeStack: Array[Node[A]] =
      new Array(Node.MaxDepth + 1)

    private val nodeIndices: Array[Int] =
      new Array(Node.MaxDepth + 1)

    def this(rootNode: Node[A]) = {
      this()
      pushNode(rootNode)
      getMoreValues()
    }

    final private def pushNode(node: Node[A]): Unit = {
      currentDepth += 1
      nodeStack(currentDepth) = node
      nodeIndices(currentDepth) = node.nodeElements - 1
    }

    final private def pushValues(node: Node[A]): Unit = {
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

    final override def next(): A = {
      if (!hasNext) throw new NoSuchElementException
      val value = currentNode.getValue(currentValuesIndex)
      currentValuesIndex -= 1
      value
    }
  }

  implicit val catsDataUnorderedFoldableForHashSet: UnorderedFoldable[HashSet] =
    new UnorderedFoldable[HashSet] {
      def unorderedFoldMap[B, C](fa: HashSet[B])(f: B => C)(implicit C: CommutativeMonoid[C]): C =
        fa.iterator.foldLeft(C.empty)((c, b) => C.combine(c, f(b)))
    }

  implicit def catsDataCommutativeMonoidForHashSet[A](implicit hash: Hash[A]): CommutativeMonoid[HashSet[A]] =
    new CommutativeMonoid[HashSet[A]] {
      def empty: HashSet[A] = HashSet.empty[A]
      def combine(x: HashSet[A], y: HashSet[A]): HashSet[A] = x.union(y)
    }

  implicit def catsDataShowForHashSet[A](implicit A: Show[A]): Show[HashSet[A]] =
    Show.show[HashSet[A]](_.show)

  implicit def catsDataHashForHashSet[A](implicit A: Hash[A]): Hash[HashSet[A]] =
    new Hash[HashSet[A]] {
      def hash(hs: HashSet[A]): Int = hs.hashCode
      def eqv(x: HashSet[A], y: HashSet[A]): Boolean = x === y
    }
}
