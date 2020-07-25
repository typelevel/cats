package cats
package data

import NonEmptyChainImpl.create
import cats.kernel._
import scala.collection.immutable.SortedMap

private[data] object NonEmptyChainImpl extends NonEmptyChainInstances with ScalaVersionSpecificNonEmptyChainImpl {
  // The following 3 types are components of a technique to
  // create a no-boxing newtype. It's coped from the
  // newtypes lib by @alexknvl
  // For more detail see https://github.com/alexknvl/newtypes
  private[data] type Base
  private[data] trait Tag extends Any
  /* aliased in data package as NonEmptyChain */
  type Type[+A] <: Base with Tag

  private[data] def create[A](s: Chain[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[data] def unwrap[A](s: Type[A]): Chain[A] =
    s.asInstanceOf[Chain[A]]

  def fromChain[A](as: Chain[A]): Option[NonEmptyChain[A]] =
    if (as.nonEmpty) Option(create(as)) else None

  def fromChainUnsafe[A](chain: Chain[A]): NonEmptyChain[A] =
    if (chain.nonEmpty) create(chain)
    else throw new IllegalArgumentException("Cannot create NonEmptyChain from empty chain")

  def fromNonEmptyList[A](as: NonEmptyList[A]): NonEmptyChain[A] =
    create(Chain.fromSeq(as.toList))

  def fromNonEmptyVector[A](as: NonEmptyVector[A]): NonEmptyChain[A] =
    create(Chain.fromSeq(as.toVector))

  def fromSeq[A](as: scala.collection.immutable.Seq[A]): Option[NonEmptyChain[A]] =
    if (as.nonEmpty) Option(create(Chain.fromSeq(as))) else None

  def fromChainPrepend[A](a: A, ca: Chain[A]): NonEmptyChain[A] =
    create(a +: ca)

  def fromChainAppend[A](ca: Chain[A], a: A): NonEmptyChain[A] =
    create(ca :+ a)

  def apply[A](a: A, as: A*): NonEmptyChain[A] =
    create(Chain.concat(Chain.one(a), Chain.fromSeq(as)))

  def one[A](a: A): NonEmptyChain[A] = create(Chain.one(a))

  implicit def catsNonEmptyChainOps[A](value: NonEmptyChain[A]): NonEmptyChainOps[A] =
    new NonEmptyChainOps(value)
}

class NonEmptyChainOps[A](private val value: NonEmptyChain[A])
    extends AnyVal
    with NonEmptyCollection[A, Chain, NonEmptyChain] {

  /**
   * Converts this chain to a `Chain`
   */
  final def toChain: Chain[A] = NonEmptyChainImpl.unwrap(value)

  /**
   * Returns a new NonEmptyChain consisting of `a` followed by this. O(1) runtime.
   */
  final def prepend[A2 >: A](a: A2): NonEmptyChain[A2] =
    create(toChain.prepend(a))

  /**
   * Alias for [[prepend]].
   */
  final def +:[A2 >: A](a: A2): NonEmptyChain[A2] =
    prepend(a)

  /**
   * Returns a new Chain consisting of this followed by `a`. O(1) runtime.
   */
  final def append[A2 >: A](a: A2): NonEmptyChain[A2] =
    create(toChain.append(a))

  /**
   * Alias for [[append]].
   */
  final def :+[A2 >: A](a: A2): NonEmptyChain[A2] =
    append(a)

  /**
   * Concatenates this with `c` in O(1) runtime.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(1, 2, 4, 5)
   * scala> nec ++ NonEmptyChain(7, 8)
   * res0: cats.data.NonEmptyChain[Int] = Chain(1, 2, 4, 5, 7, 8)
   * }}}
   */
  final def concat[A2 >: A](c: NonEmptyChain[A2]): NonEmptyChain[A2] =
    create(toChain ++ c.toChain)

  /**
   * Alias for concat
   */
  final def ++[A2 >: A](c: NonEmptyChain[A2]): NonEmptyChain[A2] =
    concat(c)

  /**
   * Appends the given chain in O(1) runtime.
   */
  final def appendChain[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
    if (c.isEmpty) value
    else create(toChain ++ c)

  /**
   * Alias for `appendChain`
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(1, 2, 4, 5)
   * scala> nec :++ Chain(3, 6, 9)
   * res0: cats.data.NonEmptyChain[Int] = Chain(1, 2, 4, 5, 3, 6, 9)
   * }}}
   */
  final def :++[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
    appendChain(c)

  /**
   * Prepends the given chain in O(1) runtime.
   */
  final def prependChain[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
    if (c.isEmpty) value
    else create(c ++ toChain)

  /**
   * Alias for `prependChain`
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(4, 5, 6)
   * scala> Chain(1, 2, 3) ++: nec
   * res0: cats.data.NonEmptyChain[Int] = Chain(1, 2, 3, 4, 5, 6)
   * }}}
   */
  final def ++:[A2 >: A](c: Chain[A2]): NonEmptyChain[A2] =
    prependChain(c)

  /**
   * Yields to Some(a, Chain[A]) with `a` removed where `f` holds for the first time,
   * otherwise yields None, if `a` was not found
   * Traverses only until `a` is found.
   */
  final def deleteFirst(f: A => Boolean): Option[(A, Chain[A])] =
    toChain.deleteFirst(f)

  /**
   * Converts this chain to a `NonEmptyList`.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(1, 2, 3, 4, 5)
   * scala> nec.toNonEmptyList
   * res0: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5)
   * }}}
   */
  final def toNonEmptyList: NonEmptyList[A] = NonEmptyList.fromListUnsafe(toChain.toList)

  /**
   * Converts this chain to a `NonEmptyVector`.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(1, 2, 3, 4, 5)
   * scala> nec.toNonEmptyVector
   * res0: cats.data.NonEmptyVector[Int] = NonEmptyVector(1, 2, 3, 4, 5)
   * }}}
   */
  final def toNonEmptyVector: NonEmptyVector[A] = NonEmptyVector.fromVectorUnsafe(toChain.toVector)

  /**
   * Returns the head and tail of this NonEmptyChain. Amortized O(1).
   */
  final def uncons: (A, Chain[A]) = toChain.uncons.get

  /**
   * Returns the init and last of this NonEmptyChain. Amortized O(1).
   */
  final def initLast: (Chain[A], A) = toChain.initLast.get

  /**
   * Returns the first element of this NonEmptyChain. Amortized O(1).
   */
  final def head: A = uncons._1

  /**
   * Returns all but the first element of this NonEmptyChain. Amortized O(1).
   */
  final def tail: Chain[A] = uncons._2

  /**
   * Returns all but the last element of this NonEmptyChain. Amortized O(1).
   */
  final def init: Chain[A] = initLast._1

  /**
   * Returns the last element of this NonEmptyChain. Amortized O(1).
   */
  final def last: A = initLast._2

  /**
   * Tests if some element is contained in this chain.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> import cats.implicits._
   * scala> val nec = NonEmptyChain(4, 5, 6)
   * scala> nec.contains(5)
   * res0: Boolean = true
   * }}}
   */
  final def contains(a: A)(implicit A: Eq[A]): Boolean = toChain.contains(a)

  /**
   * Tests whether a predicate holds for all elements of this chain.
   */
  final def forall(p: A => Boolean): Boolean = toChain.forall(p)

  /**
   * Tests whether a predicate holds for at least one element of this chain.
   */
  final def exists(f: A => Boolean): Boolean = toChain.exists(f)

  /**
   * Returns the first value that matches the given predicate.
   */
  final def find(f: A => Boolean): Option[A] = toChain.find(f)

  /**
   * Returns a new `Chain` containing all elements where the result of `pf` is final defined.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> import cats.implicits._
   * scala> val nec = NonEmptyChain(4, 5, 6).map(n => if (n % 2 == 0) Some(n) else None)
   * scala> nec.collect { case Some(n) => n }
   * res0: cats.data.Chain[Int] = Chain(4, 6)
   * }}}
   */
  final def collect[B](pf: PartialFunction[A, B]): Chain[B] = toChain.collect(pf)

  /**
   * Finds the first element of this `NonEmptyChain` for which the given partial
   * function is defined, and applies the partial function to it.
   */
  final def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = toChain.collectFirst(pf)

  /**
   * Like `collectFirst` from `scala.collection.Traversable` but takes `A => Option[B]`
   * instead of `PartialFunction`s.
   */
  final def collectFirstSome[B](f: A => Option[B]): Option[B] = toChain.collectFirstSome(f)

  /**
   * Filters all elements of this chain that do not satisfy the given predicate.
   */
  final def filter(p: A => Boolean): Chain[A] = toChain.filter(p)

  /**
   * Filters all elements of this chain that satisfy the given predicate.
   */
  final def filterNot(p: A => Boolean): Chain[A] = filter(t => !p(t))

  /**
   * Left-associative fold using f.
   */
  final def foldLeft[B](b: B)(f: (B, A) => B): B =
    toChain.foldLeft(b)(f)

  /**
   * Right-associative fold using f.
   */
  final def foldRight[B](z: B)(f: (A, B) => B): B =
    toChain.foldRight(z)(f)

  /**
   * Left-associative reduce using f.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(4, 5, 6)
   * scala> nec.reduceLeft(_ + _)
   * res0: Int = 15
   * }}}
   */
  final def reduceLeft(f: (A, A) => A): A = {
    val iter = toChain.iterator
    var result = iter.next
    while (iter.hasNext) { result = f(result, iter.next) }
    result
  }

  /**
   * Apply `f` to the "initial element" of this chain and lazily combine it
   * with every other value using the given function `g`.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(4, 5, 6)
   * scala> nec.reduceLeftTo(_.toString)((acc, cur) => acc + cur.toString)
   * res0: String = 456
   * }}}
   */
  final def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = {
    val iter = toChain.iterator
    var result = f(iter.next)
    while (iter.hasNext) { result = g(result, iter.next) }
    result
  }

  /**
   * Right-associative reduce using f.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(4, 5, 6)
   * scala> nec.reduceRight(_ + _)
   * res0: Int = 15
   * }}}
   */
  final def reduceRight(f: (A, A) => A): A = {
    val iter = toChain.reverseIterator
    var result = iter.next
    while (iter.hasNext) { result = f(result, iter.next) }
    result
  }

  /**
   * Apply `f` to the "initial element" of this chain and lazily combine it
   * with every other value using the given function `g`.
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val nec = NonEmptyChain(4, 5, 6)
   * scala> nec.reduceRightTo(_.toString)((cur, acc) => acc + cur.toString)
   * res0: String = 654
   * }}}
   */
  final def reduceRightTo[B](f: A => B)(g: (A, B) => B): B = {
    val iter = toChain.reverseIterator
    var result = f(iter.next)
    while (iter.hasNext) { result = g(iter.next, result) }
    result
  }

  /**
   * Reduce using the Semigroup of A
   */
  final def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(iterator).get

  /**
   * Applies the supplied function to each element and returns a new NonEmptyChain.
   */
  final def map[B](f: A => B): NonEmptyChain[B] =
    create(toChain.map(f))

  /**
   * Applies the supplied function to each element and returns a new NonEmptyChain from the concatenated results
   */
  final def flatMap[B](f: A => NonEmptyChain[B]): NonEmptyChain[B] =
    create(toChain.flatMap(a => f(a).toChain))

  /**
   * Returns the number of elements in this chain.
   */
  final def length: Long = toChain.size

  /**
   * Zips this `NonEmptyChain` with another `NonEmptyChain` and applies a function for each pair of elements.
   *
   * {{{
   * scala> import cats.data.NonEmptyChain
   * scala> val as = NonEmptyChain(1, 2, 3)
   * scala> val bs = NonEmptyChain("A", "B", "C")
   * scala> as.zipWith(bs)(_.toString + _)
   * res0: cats.data.NonEmptyChain[String] = Chain(1A, 2B, 3C)
   * }}}
   */
  final def zipWith[B, C](b: NonEmptyChain[B])(f: (A, B) => C): NonEmptyChain[C] =
    create(toChain.zipWith(b.toChain)(f))

  /**
   * Groups elements inside this `NonEmptyChain` according to the `Order`
   * of the keys produced by the given mapping function.
   */
  final def groupBy[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptyChain[A]] =
    toChain.groupBy(f).asInstanceOf[NonEmptyMap[B, NonEmptyChain[A]]]

  final def groupByNem[B](f: A => B)(implicit B: Order[B]): NonEmptyMap[B, NonEmptyChain[A]] = groupBy(f)

  final def iterator: Iterator[A] = toChain.iterator

  final def reverseIterator: Iterator[A] = toChain.reverseIterator

  /**
   * Reverses this `NonEmptyChain`
   */
  final def reverse: NonEmptyChain[A] =
    create(toChain.reverse)

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  final def distinct[AA >: A](implicit O: Order[AA]): NonEmptyChain[AA] =
    create(toChain.distinct[AA])

  final def sortBy[B](f: A => B)(implicit B: Order[B]): NonEmptyChain[A] = create(toChain.sortBy(f))
  final def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyChain[AA] = create(toChain.sorted[AA])
  final def toNem[T, V](implicit ev: A <:< (T, V), order: Order[T]): NonEmptyMap[T, V] =
    NonEmptyMap.fromMapUnsafe(SortedMap(toChain.toVector.map(ev): _*)(order.toOrdering))
  final def toNes[B >: A](implicit order: Order[B]): NonEmptySet[B] = NonEmptySet.of(head, tail.toVector: _*)
  final def zipWithIndex: NonEmptyChain[(A, Int)] = create(toChain.zipWithIndex)

  final def show[AA >: A](implicit AA: Show[AA]): String = s"NonEmpty${Show[Chain[AA]].show(toChain)}"
}

sealed abstract private[data] class NonEmptyChainInstances extends NonEmptyChainInstances1 {

  implicit val catsDataInstancesForNonEmptyChain: SemigroupK[NonEmptyChain]
    with NonEmptyTraverse[NonEmptyChain]
    with Bimonad[NonEmptyChain]
    with Align[NonEmptyChain] =
    new AbstractNonEmptyInstances[Chain, NonEmptyChain] with Align[NonEmptyChain] {
      def extract[A](fa: NonEmptyChain[A]): A = fa.head

      def nonEmptyTraverse[G[_]: Apply, A, B](fa: NonEmptyChain[A])(f: A => G[B]): G[NonEmptyChain[B]] = {
        def loop(head: A, tail: Chain[A]): Eval[G[NonEmptyChain[B]]] =
          tail.uncons.fold(Eval.now(Apply[G].map(f(head))(NonEmptyChain(_)))) {
            case (h, t) => Apply[G].map2Eval(f(head), Eval.defer(loop(h, t)))((b, acc) => b +: acc)
          }

        loop(fa.head, fa.tail).value
      }

      override def size[A](fa: NonEmptyChain[A]): Long = fa.length

      override def reduceLeft[A](fa: NonEmptyChain[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyChain[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      def reduceLeftTo[A, B](fa: NonEmptyChain[A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyChain[A])(f: A => B)(g: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
        Eval.defer(fa.reduceRightTo(a => Eval.now(f(a))) { (a, b) =>
          Eval.defer(g(a, b))
        })

      override def get[A](fa: NonEmptyChain[A])(idx: Long): Option[A] =
        if (idx == 0) Some(fa.head) else fa.tail.get(idx - 1)

      private val alignInstance = Align[Chain].asInstanceOf[Align[NonEmptyChain]]

      def functor: Functor[NonEmptyChain] = alignInstance.functor

      def align[A, B](fa: NonEmptyChain[A], fb: NonEmptyChain[B]): NonEmptyChain[Ior[A, B]] =
        alignInstance.align(fa, fb)

      override def alignWith[A, B, C](fa: NonEmptyChain[A], fb: NonEmptyChain[B])(f: Ior[A, B] => C): NonEmptyChain[C] =
        alignInstance.alignWith(fa, fb)(f)

      override def toNonEmptyList[A](fa: NonEmptyChain[A]): NonEmptyList[A] = fa.toNonEmptyList
    }

  implicit def catsDataOrderForNonEmptyChain[A: Order]: Order[NonEmptyChain[A]] =
    Order[Chain[A]].asInstanceOf[Order[NonEmptyChain[A]]]

  implicit def catsDataSemigroupForNonEmptyChain[A]: Semigroup[NonEmptyChain[A]] =
    Semigroup[Chain[A]].asInstanceOf[Semigroup[NonEmptyChain[A]]]

  implicit def catsDataShowForNonEmptyChain[A](implicit A: Show[A]): Show[NonEmptyChain[A]] =
    Show.show[NonEmptyChain[A]](_.show)

}

sealed abstract private[data] class NonEmptyChainInstances1 extends NonEmptyChainInstances2 {
  implicit def catsDataHashForNonEmptyChain[A: Hash]: Hash[NonEmptyChain[A]] =
    Hash[Chain[A]].asInstanceOf[Hash[NonEmptyChain[A]]]
}

sealed abstract private[data] class NonEmptyChainInstances2 extends NonEmptyChainInstances3 {
  implicit def catsDataPartialOrderForNonEmptyChain[A: PartialOrder]: PartialOrder[NonEmptyChain[A]] =
    PartialOrder[Chain[A]].asInstanceOf[PartialOrder[NonEmptyChain[A]]]
}

sealed abstract private[data] class NonEmptyChainInstances3 {
  implicit def catsDataEqForNonEmptyChain[A: Eq]: Eq[NonEmptyChain[A]] =
    Eq[Chain[A]].asInstanceOf[Eq[NonEmptyChain[A]]]
}
