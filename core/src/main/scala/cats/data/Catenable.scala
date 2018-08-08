package cats
package data

import cats.implicits._
import Catenable._

import scala.annotation.tailrec

/**
 * Trivial catenable sequence. Supports O(1) append, and (amortized)
 * O(1) `uncons`, such that walking the sequence via N successive `uncons`
 * steps takes O(N). Like a difference list, conversion to a `Seq[A]`
 * takes linear time, regardless of how the sequence is built up.
 */
sealed abstract class Catenable[+A] {

  /** Returns the head and tail of this catenable if non empty, none otherwise. Amortized O(1). */
  final def uncons: Option[(A, Catenable[A])] = {
    var c: Catenable[A] = this
    val rights = new collection.mutable.ArrayBuffer[Catenable[A]]
    // scalastyle:off null
    var result: Option[(A, Catenable[A])] = null
    while (result eq null) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            result = None
          } else {
            c = rights.last
            rights.trimEnd(1)
          }
        case Singleton(a) =>
          val next =
            if (rights.isEmpty) nil
            else rights.reduceLeft((x, y) => Append(y, x))
          result = Some(a -> next)
        case Append(l, r) => c = l; rights += r
      }
    }
    // scalastyle:on null
    result
  }

  /** Returns true if there are no elements in this collection. */
  def isEmpty: Boolean

  /** Returns false if there are no elements in this collection. */
  def nonEmpty: Boolean = !isEmpty

  /** Concatenates this with `c` in O(1) runtime. */
  final def ++[A2 >: A](c: Catenable[A2]): Catenable[A2] =
    append(this, c)

  /** Returns a new catenable consisting of `a` followed by this. O(1) runtime. */
  final def cons[A2 >: A](a: A2): Catenable[A2] =
    append(singleton(a), this)

  /** Alias for [[cons]]. */
  final def +:[A2 >: A](a: A2): Catenable[A2] =
    cons(a)

  /** Returns a new catenable consisting of this followed by `a`. O(1) runtime. */
  final def snoc[A2 >: A](a: A2): Catenable[A2] =
    append(this, singleton(a))

  /** Alias for [[snoc]]. */
  final def :+[A2 >: A](a: A2): Catenable[A2] =
    snoc(a)

  /** Applies the supplied function to each element and returns a new catenable. */
  final def map[B](f: A => B): Catenable[B] =
    foldLeft(nil: Catenable[B])((acc, a) => acc :+ f(a))

  /** Applies the supplied function to each element and returns a new catenable from the concatenated results */
  final def flatMap[B](f: A => Catenable[B]): Catenable[B] =
    foldLeft(nil: Catenable[B])((acc, a) => acc ++ f(a))

  /** Folds over the elements from left to right using the supplied initial value and function. */
  final def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var result = z
    foreach(a => result = f(result, a))
    result
  }

  /** Collect `B` from this for which `f` is defined */
  final def collect[B](pf: PartialFunction[A, B]): Catenable[B] =
    foldLeft(Catenable.nil: Catenable[B]) { (acc, a) =>
      // trick from TraversableOnce, used to avoid calling both isDefined and apply (or calling lift)
      val x = pf.applyOrElse(a, sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) acc :+ x.asInstanceOf[B]
      else acc
    }

  /** Remove elements not matching the predicate */
  final def filter(f: A => Boolean): Catenable[A] =
    collect { case a if f(a) => a }

  /** Remove elements matching the predicate */
  final def filterNot(f: A => Boolean): Catenable[A] =
    filter(a => !f(a))

  /** Find the first element matching the predicate, if one exists */
  final def find(f: A => Boolean): Option[A] = {
    var result: Option[A] = Option.empty[A]
    foreachUntil { a =>
      val b = f(a)
      if (b) result = Option(a)
      b
    }
    result
  }

  /** Check whether at least one element satisfies the predicate */
  final def exists(f: A => Boolean): Boolean = {
    var result: Boolean = false
    foreachUntil { a =>
      val b = f(a)
      if (b) result = true
      b
    }
    result
  }

  /** Check whether all elements satisfy the predicate */
  final def forall(f: A => Boolean): Boolean =
    exists(a => !f(a))

  final def contains[AA >: A](a: AA)(implicit A: Eq[AA]): Boolean =
    exists(A.eqv(a, _))


  /**
   * Yields to Some(a, Catenable[A]) with `a` removed where `f` holds for the first time,
   * otherwise yields None, if `a` was not found
   * Traverses only until `a` is found.
   */
  final def deleteFirst(f: A => Boolean): Option[(A, Catenable[A])] = {
    @tailrec
    def go(rem: Catenable[A], acc: Catenable[A]): Option[(A, Catenable[A])] =
      rem.uncons match {
        case Some((a, tail)) =>
          if (!f(a)) go(tail, acc :+ a)
          else Some((a, acc ++ tail))

        case None => None
      }
    go(this, Catenable.nil)
  }

  /** Applies the supplied function to each element, left to right. */
  private final def foreach(f: A => Unit): Unit =
    foreachUntil { a => f(a); false }

  /** Applies the supplied function to each element, left to right, but stops when true is returned */
  private final def foreachUntil(f: A => Boolean): Unit = {
    var c: Catenable[A] = this
    val rights = new collection.mutable.ArrayBuffer[Catenable[A]]
    // scalastyle:off null
    while (c ne null) {
      c match {
        case Empty =>
          if (rights.isEmpty) {
            c = null
          } else {
            c = rights.last
            rights.trimEnd(1)
          }
        case Singleton(a) =>
          val b = f(a)
          if (b) return ();
          c =
            if (rights.isEmpty) Empty
            else rights.reduceLeft((x, y) => Append(y, x))
          rights.clear()
        case Append(l, r) => c = l; rights += r
      }
    }
    // scalastyle:on null
  }

  /** Returns the number of elements in this structure */
  final def length: Int = {
    var i: Int = 0
    foreach(_ => i += 1)
    i
  }

  /** Alias for length */
  final def size: Int = length


  /** Converts to a list. */
  final def toList: List[A] = {
    val builder = List.newBuilder[A]
    foreach { a =>
      builder += a; ()
    }
    builder.result
  }

  /** Converts to a vector. */
  final def toVector: Vector[A] = {
    val builder = new scala.collection.immutable.VectorBuilder[A]()
    foreach { a =>
      builder += a; ()
    }
    builder.result
  }

  def show[AA >: A](implicit AA: Show[AA]): String = {
    val builder = new StringBuilder("Catenable(")
    var first = true

    foreach { a =>
      if (first) { builder ++= AA.show(a); first = false }
      else builder ++= ", " + AA.show(a)
      ()
    }
    builder += ')'
    builder.result
  }

  override def toString: String = show(Show.show[A](_.toString))
}

object Catenable extends CatenableInstances {

  private val sentinel: Function1[Any, Any] = new scala.runtime.AbstractFunction1[Any, Any]{ def apply(a: Any) = this }

  private[data] final case object Empty extends Catenable[Nothing] {
    def isEmpty: Boolean = true
  }
  private[data] final case class Singleton[A](a: A) extends Catenable[A] {
    def isEmpty: Boolean = false
  }
  private[data] final case class Append[A](left: Catenable[A], right: Catenable[A])
    extends Catenable[A] {
    def isEmpty: Boolean =
      false // b/c `append` constructor doesn't allow either branch to be empty
  }

  /** Empty catenable. */
  val nil: Catenable[Nothing] = Empty

  def empty[A]: Catenable[A] = nil

  /** Creates a catenable of 1 element. */
  def singleton[A](a: A): Catenable[A] = Singleton(a)

  /** Appends two catenables. */
  def append[A](c: Catenable[A], c2: Catenable[A]): Catenable[A] =
    if (c.isEmpty) c2
    else if (c2.isEmpty) c
    else Append(c, c2)

  /** Creates a catenable from the specified sequence. */
  def fromSeq[A](s: Seq[A]): Catenable[A] =
    if (s.isEmpty) nil
    else s.view.reverse.map(singleton).reduceLeft((x, y) => Append(y, x))

  /** Creates a catenable from the specified elements. */
  def apply[A](as: A*): Catenable[A] =
    as match {
      case w: collection.mutable.WrappedArray[A] =>
        if (w.isEmpty) nil
        else if (w.size == 1) singleton(w.head)
        else {
          val arr: Array[A] = w.array
          var c: Catenable[A] = singleton(arr.last)
          var idx = arr.size - 2
          while (idx >= 0) {
            c = Append(singleton(arr(idx)), c)
            idx -= 1
          }
          c
        }
      case _ => fromSeq(as)
    }
}

private[data] sealed abstract class CatenableInstances {
  implicit def catsDataMonoidForCatenable[A]: Monoid[Catenable[A]] = new Monoid[Catenable[A]] {
    def empty: Catenable[A] = Catenable.nil
    def combine(c: Catenable[A], c2: Catenable[A]): Catenable[A] = Catenable.append(c, c2)
  }

  implicit val catsDataInstancesForCatenable: Traverse[Catenable] with Alternative[Catenable] with Monad[Catenable] =
    new Traverse[Catenable] with Alternative[Catenable] with Monad[Catenable] {
      def foldLeft[A, B](fa: Catenable[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
      def foldRight[A, B](fa: Catenable[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable[List].foldRight(fa.toList, b)(f)

      override def map[A, B](fa: Catenable[A])(f: A => B): Catenable[B] = fa.map(f)
      override def toList[A](fa: Catenable[A]): List[A] = fa.toList
      override def isEmpty[A](fa: Catenable[A]): Boolean = fa.isEmpty
      def traverse[F[_], A, B](fa: Catenable[A])(f: A => F[B])(
        implicit G: Applicative[F]): F[Catenable[B]] =
        Traverse[List].traverse(fa.toList)(f).map(Catenable.apply)
      def empty[A]: Catenable[A] = Catenable.nil
      def combineK[A](c: Catenable[A], c2: Catenable[A]): Catenable[A] = Catenable.append(c, c2)
      def pure[A](a: A): Catenable[A] = Catenable.singleton(a)
      def flatMap[A, B](fa: Catenable[A])(f: A => Catenable[B]): Catenable[B] =
        fa.flatMap(f)
      def tailRecM[A, B](a: A)(f: A => Catenable[Either[A, B]]): Catenable[B] = {
        var acc: Catenable[B] = Catenable.nil
        @tailrec def go(rest: List[Catenable[Either[A, B]]]): Unit =
          rest match {
            case hd :: tl =>
              hd.uncons match {
                case Some((hdh, hdt)) =>
                  hdh match {
                    case Right(b) =>
                      acc = acc :+ b
                      go(hdt :: tl)
                    case Left(a) =>
                      go(f(a) :: hdt :: tl)
                  }
                case None =>
                  go(tl)
              }
            case _ => ()
          }
        go(f(a) :: Nil)
        acc
      }
    }

  implicit def catsDataShowForCatenable[A](implicit A: Show[A]): Show[Catenable[A]] =
    Show.show[Catenable[A]](_.show)

  implicit def catsDataEqForCatenable[A](implicit A: Eq[A]): Eq[Catenable[A]] = new Eq[Catenable[A]] {
    def eqv(x: Catenable[A], y: Catenable[A]): Boolean =
      (x eq y) || x.toList === y.toList
  }

}
