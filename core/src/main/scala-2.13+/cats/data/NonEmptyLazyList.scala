package cats.data

import NonEmptyLazyListImpl.{NonEmptyLazyList, create, unwrap}
import cats.{Apply, Bimonad, Eval, Foldable, Monad, NonEmptyReducible, NonEmptyTraverse, Order, Reducible, SemigroupK, Show}
import cats.kernel.{Order, _}

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.collection.mutable.ListBuffer

private[data] object NonEmptyLazyListImpl extends NonEmptyLazyListInstances {
  // TODO belongs in package object:
  type NonEmptyLazyList[+A] = NonEmptyLazyListImpl.Type[A]
  //val NonEmptyLazyList = NonEmptyChainImpl

  private[data] type Base
  private[data] trait Tag extends Any
  type Type[+A] <: Base with Tag

  private[cats] def create[A](s: LazyList[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[cats] def unwrap[A](s: Type[A]): LazyList[A] =
    s.asInstanceOf[LazyList[A]]

  def fromLazyList[A](as: LazyList[A]): Option[NonEmptyLazyList[A]] =
    if (as.nonEmpty) Option(create(as)) else None

  def fromLazyListUnsafe[A](ll: LazyList[A]): NonEmptyLazyList[A] =
    if (ll.nonEmpty) create(ll)
    else throw new IllegalArgumentException("Cannot create NonEmptyLazyList from empty LazyList")

  def fromNonEmptyList[A](as: NonEmptyList[A]): NonEmptyLazyList[A] =
    create(LazyList.from(as.toList))

  def fromNonEmptyVector[A](as: NonEmptyVector[A]): NonEmptyLazyList[A] =
    create(LazyList.from(as.toVector))

  def fromSeq[A](as: Seq[A]): Option[NonEmptyLazyList[A]] =
    if (as.nonEmpty) Option(create(LazyList.from(as))) else None

  def fromLazyListPrepend[A](a: A, ca: LazyList[A]): NonEmptyLazyList[A] =
    create(a +: ca)

  def fromLazyListAppend[A](ca: LazyList[A], a: A): NonEmptyLazyList[A] =
    create(ca :+ a)

  def apply[A](a: => A, as: A*): NonEmptyLazyList[A] =
    create(LazyList.concat(LazyList(a), LazyList.from(as)))

  implicit def catsNonEmptyLazyListOps[A](value: NonEmptyLazyList[A]): NonEmptyLazyListOps[A] =
    new NonEmptyLazyListOps(value)
}

class NonEmptyLazyListOps[A](private val value: NonEmptyLazyList[A]) extends AnyVal {

  /**
    * Converts this NonEmptyLazyList to a `LazyList`
    */
  final def toLazyList: LazyList[A] = NonEmptyLazyListImpl.unwrap(value)

  /**
    * Returns the last element
    */
  final def last: A = toLazyList.last

  /**
    * Returns all elements but the last
    */
  final def init: LazyList[A] = toLazyList.init

  /**
    * Returns the size of this NonEmptyLazyList
    */
  final def size: Int = toLazyList.size

  /**
    * Returns the length of this NonEmptyLazyList
    */
  final def length: Int = size

  /**
    * Returns a new NonEmptyLazyList consisting of `a` followed by this
    */
  final def prepend[AA >: A](a: AA): NonEmptyLazyList[AA] =
    create(a #:: toLazyList)

  /**
    * Alias for [[prepend]].
    */
  final def +:[AA >: A](a: AA): NonEmptyLazyList[AA] =
    prepend(a)

  /**
    * Alias for [[prepend]].
    */
  final def #::[AA >: A](a: AA): NonEmptyLazyList[AA] =
    prepend(a)

  /**
    * Returns a new NonEmptyLazyList consisting of this followed by `a`
    */
  final def append[AA >: A](a: AA): NonEmptyLazyList[AA] =
    create(toLazyList :+ a)

  /**
    * Alias for [[append]].
    */
  final def :+[AA >: A](a: AA): NonEmptyLazyList[AA] =
    append(a)

  /**
    * concatenates this with `ll`
    */
  final def concat[AA >: A](ll: LazyList[AA]): NonEmptyLazyList[AA] =
    create(toLazyList ++ ll)

  /**
    * Concatenates this with `nell`
    */
  final def concatNell[AA >: A](nell: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    create(toLazyList ++ nell.toLazyList)

  /**
    * Alias for concatNell
    */
  final def ++[AA >: A](nell: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    concatNell(nell)

  /**
    * Appends the given LazyList
    */
  final def appendLazyList[AA >: A](nell: LazyList[AA]): NonEmptyLazyList[AA] =
    if (nell.isEmpty) value
    else create(toLazyList ++ nell)

  /**
    * Alias for `appendLazyList`
    */
  final def :++[AA >: A](c: LazyList[AA]): NonEmptyLazyList[AA] =
    appendLazyList(c)

  /**
    * Prepends the given LazyList
    */
  final def prependLazyList[AA >: A](c: LazyList[AA]): NonEmptyLazyList[AA] =
    if (c.isEmpty) value
    else create(c ++ toLazyList)

  /**
    * Prepends the given NonEmptyLazyList
    */
  final def prependNell[AA >: A](c: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    create(c.toLazyList ++ toLazyList)

  /**
    * Alias for `prependNell`
    */
  final def ++:[AA >: A](c: NonEmptyLazyList[AA]): NonEmptyLazyList[AA] =
    prependNell(c)

  /**
    * Converts this NonEmptyLazyList to a `NonEmptyList`.
    */ // TODO also add toNonEmptyLazyList to NonEmptyList?
  final def toNonEmptyList: NonEmptyList[A] =
    NonEmptyList.fromListUnsafe(toLazyList.toList)

  /**
    * Converts this LazyList to a `NonEmptyVector`.
    */
  final def toNonEmptyVector: NonEmptyVector[A] =
    NonEmptyVector.fromVectorUnsafe(toLazyList.toVector)

  /**
    * Returns the head and tail of this NonEmptyLazyList
    */
  final def uncons: (A, LazyList[A]) =
    (head, tail)

  /**
    * Returns the first element
    */
  final def head: A = uncons._1

  /**
    * Returns all but the first element
    */
  final def tail: LazyList[A] = uncons._2

  /**
    * Tests if some element is contained in this NonEmptyLazyList
    */
  final def contains(a: A)(implicit A: Eq[A]): Boolean =
    toLazyList.contains(a)

  /**
    * Tests whether a predicate holds for all elements
    */
  final def forall(p: A => Boolean): Boolean =
    toLazyList.forall(p)

  /**
    * Tests whether a predicate holds for at least one element of this LazyList
    */
  final def exists(f: A => Boolean): Boolean =
    toLazyList.exists(f)

  /**
    * Returns the first value that matches the given predicate.
    */
  final def find(f: A => Boolean): Option[A] =
    toLazyList.find(f)

  /**
    * Returns a new `LazyList` containing all elements where the result of `pf` is final defined.
    */
  final def collect[B](pf: PartialFunction[A, B]): LazyList[B] =
    toLazyList.collect(pf)

  /**
    * Finds the first element of this `NonEmptyLazyList` for which the given partial
    * function is defined, and applies the partial function to it.
    */
  final def collectLazyList[B](pf: PartialFunction[A, B]): Option[B] = toLazyList.collectFirst(pf)

  /**
    * Like `collectFirst` from `scala.collection.Traversable` but takes `A => Option[B]`
    * instead of `PartialFunction`s.
    */ // TODO manually implemented because LazyList doesn't have collectFirstSome
  final def collectFirstSome[B](f: A => Option[B]): Option[B] =
    collectLazyList(PartialFunction[A, B] {
      case a: A if f(a).isDefined => f(a).get
    })

  /**
    * Filters all elements of this NonEmptyLazyList that do not satisfy the given predicate.
    */
  final def filter(p: A => Boolean): LazyList[A] = toLazyList.filter(p)

  /**
    * Filters all elements of this NonEmptyLazyList that satisfy the given predicate.
    */
  final def filterNot(p: A => Boolean): LazyList[A] = filter(t => !p(t))

  /**
    * Left-associative fold using f.
    */
  final def foldLeft[B](b: B)(f: (B, A) => B): B =
    toLazyList.foldLeft(b)(f)

  /**
    * Right-associative fold using f.
    */
  final def foldRight[B](z: B)(f: (A, B) => B): B =
    toLazyList.foldRight(z)(f)

  /**
    * Left-associative reduce using f.
    */
  final def reduceLeft(f: (A, A) => A): A =
    toLazyList.reduceLeft(f)

  /**
    * Apply `f` to the "initial element" of this LazyList and lazily combine it
    * with every other value using the given function `g`.
    */ // TODO not implemented in LazyList
  final def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B =
    toLazyList.reduceLeftTo(f)(g)


  /**
    * Right-associative reduce using f.
    */
  final def reduceRight[AA >: A](f: (A, AA) => AA): AA =
    toLazyList.reduceRight(f)

  /**
    * Apply `f` to the "initial element" of this NonEmptyLazyList and
    * lazily combine it with every other value using the given function `g`.
    */ // TODO not implemented in LazyList
  final def reduceRightTo[B](f: A => B)(g: (A, B) => B): B =
    toLazyList.reduceRightTo(f)(g)

  /**
    * Reduce using the Semigroup of A
    */
  final def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(iterator).get

  /**
    * Applies the supplied function to each element and returns a new NonEmptyLazyList from the concatenated results
    */
  final def flatMap[B](f: A => NonEmptyLazyList[B]): NonEmptyLazyList[B] =
    create(toLazyList.flatMap(f.andThen(_.toLazyList)))

  /**
    * Returns the number of elements
    */
  final def length: Long = toLazyList.size

  /**
    * Zips this `NonEmptyLazyList` with another `NonEmptyLazyList` and applies a function for each pair of elements
    */ // TODO manually implemented
  final def zipWith[B, C](b: NonEmptyLazyList[B])(f: (A, B) => C): NonEmptyLazyList[C] =
    create(toLazyList.zip(b.toLazyList).map { case (a, b) => f(a, b) })

  /**
    * Zips each element of this `NonEmptyLazyList` with its index
    */
  final def zipWithIndex: NonEmptyLazyList[(A, Int)] =
    create(toLazyList.zipWithIndex)

  /**
    * Groups elements inside this `NonEmptyLazyList` according to the `Order`
    * of the keys produced by the given mapping function.
    */
  final def groupBy[B](f: A => B): NonEmptyMap[B, NonEmptyLazyList[A]] =
    toLazyList.groupBy(f).asInstanceOf[NonEmptyMap[B, NonEmptyLazyList[A]]]

  final def iterator: Iterator[A] = toLazyList.iterator

  final def reverseIterator: Iterator[A] = toLazyList.reverseIterator

  /**
    * Reverses this `NonEmptyLazyList`
    */
  final def reverse: NonEmptyLazyList[A] =
    create(toLazyList.reverse)

  /**
    * Remove duplicates.
    */
  final def distinct: NonEmptyLazyList[A] =
    create(toLazyList.distinct)
}

sealed abstract private[data] class NonEmptyLazyListInstances extends NonEmptyLazyListInstances1 {

  implicit val catsDataInstancesForNonEmptyLazyList: SemigroupK[NonEmptyLazyList]
    with Reducible[NonEmptyLazyList]
    with Bimonad[NonEmptyLazyList]
    with NonEmptyTraverse[NonEmptyLazyList] =
    new NonEmptyReducible[NonEmptyLazyList, LazyList] with SemigroupK[NonEmptyLazyList] with Bimonad[NonEmptyLazyList]
      with NonEmptyTraverse[NonEmptyLazyList] {

      def combineK[A](a: NonEmptyLazyList[A], b: NonEmptyLazyList[A]): NonEmptyLazyList[A] =
        a ++ b

      def pure[A](x: A): NonEmptyLazyList[A] = NonEmptyLazyList(x)

      def flatMap[A, B](fa: NonEmptyLazyList[A])(f: A => NonEmptyLazyList[B]): NonEmptyLazyList[B] =
        fa.flatMap(f)

      // TODO does this work?
      def tailRecM[A, B](a: A)(f: A => NonEmptyLazyList[Either[A, B]]): NonEmptyLazyList[B] =
        create(Monad[LazyList].tailRecM(a)(a => unwrap(f(a))))

      def extract[A](x: NonEmptyLazyList[A]): A = x.head

      // TODO preserve laziness
      def coflatMap[A, B](fa: NonEmptyLazyList[A])(f: NonEmptyLazyList[A] => B): NonEmptyLazyList[B] = {
        @tailrec def go(as: LazyList[A], res: ListBuffer[B]): LazyList[B] =
          as.uncons match {
            case Some((h, t)) => go(t, res += f(NonEmptyLazyList.fromLazyListPrepend(h, t)))
            case None         => LazyList.fromSeq(res.result())
          }
        NonEmptyLazyList.fromLazyListPrepend(f(fa), go(fa.tail, ListBuffer.empty))
      }

      def nonEmptyTraverse[G[_]: Apply, A, B](fa: NonEmptyLazyList[A])(f: A => G[B]): G[NonEmptyLazyList[B]] =
        Foldable[LazyList]
          .reduceRightToOption[A, G[LazyList[B]]](fa.tail)(a => Apply[G].map(f(a))(Lazylist.one)) { (a, lglb) =>
          Apply[G].map2Eval(f(a), lglb)(_ +: _)
        }
          .map {
            case None        => Apply[G].map(f(fa.head))(NonEmptyLazyList.apply)
            case Some(gtail) => Apply[G].map2(f(fa.head), gtail)((h, t) => create(NonEmptyLazyList(h) ++ t))
          }
          .value

      override def map[A, B](fa: NonEmptyLazyList[A])(f: A => B): NonEmptyLazyList[B] =
        create(fa.toLazyList.map(f))

      override def size[A](fa: NonEmptyLazyList[A]): Long = fa.length

      override def reduceLeft[A](fa: NonEmptyLazyList[A])(f: (A, A) => A): A =
        fa.reduceLeft(f)

      override def reduce[A](fa: NonEmptyLazyList[A])(implicit A: Semigroup[A]): A =
        fa.reduce

      def reduceLeftTo[A, B](fa: NonEmptyLazyList[A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyLazyList[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        Eval.defer(fa.reduceRightTo(a => Eval.now(f(a))) { (a, b) =>
          Eval.defer(g(a, b))
        })

      override def foldLeft[A, B](fa: NonEmptyLazyList[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: NonEmptyLazyList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable[LazyList].foldRight(fa.toLazyList, lb)(f)

      override def foldMap[A, B](fa: NonEmptyLazyList[A])(f: A => B)(implicit B: Monoid[B]): B =
        B.combineAll(fa.toLazyList.iterator.map(f))

      override def fold[A](fa: NonEmptyLazyList[A])(implicit A: Monoid[A]): A =
        fa.reduce

      override def find[A](fa: NonEmptyLazyList[A])(f: A => Boolean): Option[A] =
        fa.find(f)

      override def forall[A](fa: NonEmptyLazyList[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def exists[A](fa: NonEmptyLazyList[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def toList[A](fa: NonEmptyLazyList[A]): List[A] = fa.toLazyList.toList

      override def toNonEmptyList[A](fa: NonEmptyLazyList[A]): NonEmptyList[A] =
        fa.toNonEmptyList

      override def collectFirst[A, B](fa: NonEmptyLazyList[A])(pf: PartialFunction[A, B]): Option[B] =
        fa.collectFirst(pf)

      override def collectFirstSome[A, B](fa: NonEmptyLazyList[A])(f: A => Option[B]): Option[B] =
        fa.collectFirstSome(f)
    }

  implicit def catsDataOrderForNonEmptyLazyList[A: Order]: Order[NonEmptyLazyList[A]] =
    Order.by[NonEmptyLazyList[A], LazyList[A]](_.toLazyList)

  implicit def catsDataShowForNonEmptyLazyList[A](implicit A: Show[A]): Show[NonEmptyLazyList[A]] =
    Show.show[NonEmptyLazyList[A]](nec => s"NonEmpty${Show[LazyList[A]].show(nec.toLazyList)}")

  implicit def catsDataSemigroupForNonEmptyLazyList[A]: Semigroup[NonEmptyLazyList[A]] = new Semigroup[NonEmptyLazyList[A]] {
    def combine(x: NonEmptyLazyList[A], y: NonEmptyLazyList[A]): NonEmptyLazyList[A] = x ++ y
  }
}

sealed abstract private[data] class NonEmptyLazyListInstances1 extends NonEmptyLazyListInstances2 {
  implicit def catsDataPartialOrderForNonEmptyLazyList[A: PartialOrder]: PartialOrder[NonEmptyLazyList[A]] =
    PartialOrder.by[NonEmptyLazyList[A], LazyList[A]](_.toLazyList)
}

sealed abstract private[data] class NonEmptyLazyListInstances2 {
  implicit def catsDataEqForNonEmptyLazyList[A: Eq]: Eq[NonEmptyLazyList[A]] =
    new Eq[NonEmptyLazyList[A]] {
      def eqv(x: NonEmptyLazyList[A], y: NonEmptyLazyList[A]): Boolean = x.toLazyList === y.toLazyList
    }
}

