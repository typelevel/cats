package cats
package data

import cats.data.NonEmptyVector.ZipNonEmptyVector

import scala.collection.immutable.TreeSet
import NonEmptyVector.create
import instances.vector._

object NonEmptyVector extends NonEmptyVectorInstances with NewtypeCovariant {

  private[cats] def create[A](s: Vector[A]): Type[A] =
    s.asInstanceOf[Type[A]]

  private[cats] def unwrap[A](s: Type[A]): Vector[A] =
    s.asInstanceOf[Vector[A]]

  def fromVector[A](v: Vector[A]): Option[NonEmptyVector[A]] =
    if (v.nonEmpty) Option(create(v)) else None

  def fromNonEmptyList[A](as: NonEmptyList[A]): NonEmptyVector[A] =
    create(Vector.from(as.toList))

  def fromSeq[A](as: Seq[A]): Option[NonEmptyVector[A]] =
    if (as.nonEmpty) Option(create(Vector.from(as))) else None

  def fromVectorPrepend[A](a: A, ca: Vector[A]): NonEmptyVector[A] =
    create(a +: ca)

  def fromVectorAppend[A](ca: Vector[A], a: A): NonEmptyVector[A] =
    create(ca :+ a)


  def apply[A](head: A, tail: Vector[A]): NonEmptyVector[A] =
    create(head +: tail)

  def of[A](head: A, tail: A*): NonEmptyVector[A] = {
    val buf = Vector.newBuilder[A]
    buf += head
    tail.foreach(buf += _)
    create(buf.result)
  }

  def one[A](head: A): NonEmptyVector[A] = apply(head, Vector.empty[A])

  def unapply[A](nev: NonEmptyVector[A]): Some[(A, Vector[A])] = Some((nev.head, nev.tail))

  def fromVectorUnsafe[A](vector: Vector[A]): NonEmptyVector[A] =
    if (vector.nonEmpty) create(vector)
    else throw new IllegalArgumentException("Cannot create NonEmptyVector from empty vector")

  class ZipNonEmptyVector[A](val value: NonEmptyVector[A]) extends Serializable

  object ZipNonEmptyVector {

    def apply[A](nev: NonEmptyVector[A]): ZipNonEmptyVector[A] =
      new ZipNonEmptyVector(nev)

    implicit val catsDataCommutativeApplyForZipNonEmptyVector: CommutativeApply[ZipNonEmptyVector] =
      new CommutativeApply[ZipNonEmptyVector] {
        def ap[A, B](ff: ZipNonEmptyVector[A => B])(fa: ZipNonEmptyVector[A]): ZipNonEmptyVector[B] =
          ZipNonEmptyVector(ff.value.zipWith(fa.value)(_.apply(_)))

        override def map[A, B](fa: ZipNonEmptyVector[A])(f: (A) => B): ZipNonEmptyVector[B] =
          ZipNonEmptyVector(fa.value.map(f))

        override def product[A, B](fa: ZipNonEmptyVector[A], fb: ZipNonEmptyVector[B]): ZipNonEmptyVector[(A, B)] =
          ZipNonEmptyVector(fa.value.zipWith(fb.value) { case (a, b) => (a, b) })
      }

    implicit def zipNevEq[A: Eq]: Eq[ZipNonEmptyVector[A]] = Eq.by(_.value)
  }

  implicit def catsNonEmptyVectorOps[A](value: NonEmptyVector[A]): NonEmptyVectorOps[A] =
    new NonEmptyVectorOps(value)
}

/**
 * A data type which represents a `Vector` guaranteed to contain at least one element.
 * <br/>
 * Note that the constructor is `private` to prevent accidental construction of an empty
 * `NonEmptyVector`. However, due to https://issues.scala-lang.org/browse/SI-6601, on
 * Scala 2.10, this may be bypassed due to a compiler bug.
 */
class NonEmptyVectorOps[+A] (private val value: NonEmptyVector[A]) extends AnyVal {

  final def toVector: Vector[A] = NonEmptyVector.unwrap(value)

  /** Gets the element at the index, if it exists */
  def get(i: Int): Option[A] =
    toVector.lift(i)

  /** Gets the element at the index, or throws an exception if none exists */
  def getUnsafe(i: Int): A = toVector(i)

  /** Updates the element at the index, if it exists */
  def updated[AA >: A](i: Int, a: AA): Option[NonEmptyVector[AA]] =
    if (toVector.isDefinedAt(i)) Some(create(toVector.updated(i, a))) else None

  /**
   * Updates the element at the index, or throws an `IndexOutOfBoundsException`
   * if none exists (if `i` does not satisfy `0 <= i < length`).
   */
  def updatedUnsafe[AA >: A](i: Int, a: AA): NonEmptyVector[AA] = create(toVector.updated(i, a))

  def head: A = toVector.head

  def tail: Vector[A] = toVector.tail

  def last: A = toVector.last

  def init: Vector[A] = toVector.init

  /**
   * Remove elements not matching the predicate
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> val nev = NonEmptyVector.of(1, 2, 3, 4, 5)
   * scala> nev.filter(_ < 3)
   * res0: scala.collection.immutable.Vector[Int] = Vector(1, 2)
   * }}}
   */
  def filter(f: A => Boolean): Vector[A] = toVector.filter(f)

  /**
   * Remove elements matching the predicate
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> val nev = NonEmptyVector.of(1, 2, 3, 4, 5)
   * scala> nev.filterNot(_ < 3)
   * res0: scala.collection.immutable.Vector[Int] = Vector(3, 4, 5)
   * }}}
   */
  def filterNot(f: A => Boolean): Vector[A] = toVector.filterNot(f)

  def collect[B](pf: PartialFunction[A, B]): Vector[B] = toVector.collect(pf)

  /**
   * Alias for [[concat]]
   */
  def ++[AA >: A](other: Vector[AA]): NonEmptyVector[AA] = concat(other)

  /**
   * Append this NEV to another NEV, producing a new `NonEmptyVector`.
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> val nev = NonEmptyVector.of(1, 2, 3)
   * scala> nev ++: NonEmptyVector.of(4, 5)
   * res0: cats.data.NonEmptyVector[Int] = NonEmptyVector(1, 2, 3, 4, 5)
   * }}}
   */
  def ++:[AA >: A](other: NonEmptyVector[AA]): NonEmptyVector[AA] = other.concatNev(value)

  /**
   * Append another `Vector` to this, producing a new `NonEmptyVector`.
   */
  def concat[AA >: A](other: Vector[AA]): NonEmptyVector[AA] = create(toVector ++ other)

  /**
   * Append another `NonEmptyVector` to this, producing a new `NonEmptyVector`.
   */
  def concatNev[AA >: A](other: NonEmptyVector[AA]): NonEmptyVector[AA] = create(toVector ++ other.toVector)

  /**
   * Append an item to this, producing a new `NonEmptyVector`.
   */
  def append[AA >: A](a: AA): NonEmptyVector[AA] = create(toVector :+ a)

  /**
   * Alias for [[append]]
   */
  def :+[AA >: A](a: AA): NonEmptyVector[AA] = append(a)

  /**
   * Prepend an item to this, producing a new `NonEmptyVector`.
   */
  def prepend[AA >: A](a: AA): NonEmptyVector[AA] = create(a +: toVector)

  /**
   * Alias for [[prepend]]
   */
  def +:[AA >: A](a: AA): NonEmptyVector[AA] = prepend(a)

  /**
   * Find the first element matching the predicate, if one exists
   */
  def find(f: A => Boolean): Option[A] = toVector.find(f)

  /**
   * Check whether at least one element satisfies the predicate.
   */
  def exists(f: A => Boolean): Boolean = toVector.exists(f)

  /**
   * Check whether all elements satisfy the predicate.
   */
  def forall(f: A => Boolean): Boolean = toVector.forall(f)

  /**
   * Left-associative fold using f.
   */
  def foldLeft[B](b: B)(f: (B, A) => B): B =
    toVector.foldLeft(b)(f)

  /**
   * Right-associative fold using f.
   */
  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[Vector].foldRight(toVector, lb)(f)

  /**
   * Applies f to all the elements
   */
  def map[B](f: A => B): NonEmptyVector[B] =
    create(toVector.map(f))

  /**
   *  Applies f to all elements and combines the result
   */
  def flatMap[B](f: A => NonEmptyVector[B]): NonEmptyVector[B] =
    create(toVector.flatMap(a => f(a).toVector))

  /**
   * Left-associative reduce using f.
   */
  def reduceLeft[AA >: A](f: (AA, AA) => AA): AA =
    toVector.reduceLeft(f)

  /**
   * Apply `f` to the "initial element" of this Vector and lazily combine it
   * with every other value using the given function `g`.
   */
  final def reduceLeftTo[B](f: A => B)(g: (B, A) => B): B = {
    val iter = toVector.iterator
    var result = f(iter.next)
    while (iter.hasNext) { result = g(result, iter.next) }
    result
  }

  /**
   * Right-associative reduce using f.
   */
  final def reduceRight[AA >: A](f: (A, AA) => AA): AA =
    toVector.reduceRight(f)

  /**
   * Apply `f` to the "initial element" of this NonEmptyVector and
   * lazily combine it with every other value using the given function `g`.
   */
  final def reduceRightTo[B](f: A => B)(g: (A, B) => B): B = {
    val iter = toVector.reverseIterator
    var result = f(iter.next)
    while (iter.hasNext) { result = g(iter.next, result) }
    result
  }

  def toNonEmptyList: NonEmptyList[A] =
    NonEmptyList.fromListUnsafe(toVector.toList)

  /**
   * Reduce using the Semigroup of A
   */
  def reduce[AA >: A](implicit S: Semigroup[AA]): AA =
    S.combineAllOption(toVector).get

  /**
   * Typesafe equality operator.
   *
   * This method is similar to == except that it only allows two
   * NonEmptyVector[A] values to be compared to each other, and uses
   * equality provided by Eq[_] instances, rather than using the
   * universal equality provided by .equals.
   */
  def ===[AA >: A](that: NonEmptyVector[AA])(implicit A: Eq[AA]): Boolean =
    Eq[Vector[AA]].eqv(toVector, that.toVector)

  /**
   * Typesafe stringification method.
   *
   * This method is similar to .toString except that it stringifies
   * values according to Show[_] instances, rather than using the
   * universal .toString method.
   */
  def show[AA >: A](implicit AA: Show[AA]): String =
    s"NonEmpty${Show[Vector[AA]].show(toVector)}"

  def length: Int = toVector.length

  /**
   * Remove duplicates. Duplicates are checked using `Order[_]` instance.
   */
  def distinct[AA >: A](implicit O: Order[AA]): NonEmptyVector[AA] = {
    implicit val ord = O.toOrdering

    val buf = Vector.newBuilder[AA]
    tail.foldLeft(TreeSet(head: AA)) { (elementsSoFar, a) =>
      if (elementsSoFar(a)) elementsSoFar
      else {
        buf += a; elementsSoFar + a
      }
    }

    NonEmptyVector(head, buf.result())
  }

  /**
   * Zips this `NonEmptyVector` with another `NonEmptyVector` and applies a function for each pair of elements.
   *
   * {{{
   * scala> import cats.data.NonEmptyVector
   * scala> val as = NonEmptyVector.of(1, 2, 3)
   * scala> val bs = NonEmptyVector.of("A", "B", "C")
   * scala> as.zipWith(bs)(_ + _)
   * res0: cats.data.NonEmptyVector[String] = NonEmptyVector(1A, 2B, 3C)
   * }}}
   */
  def zipWith[B, C](b: NonEmptyVector[B])(f: (A, B) => C): NonEmptyVector[C] =
    NonEmptyVector.fromVectorUnsafe(toVector.lazyZip(b.toVector).map(f))

  def reverse: NonEmptyVector[A] =
    create(toVector.reverse)

  def zipWithIndex: NonEmptyVector[(A, Int)] =
    create(toVector.zipWithIndex)

  def sortBy[B](f: A => B)(implicit B: Order[B]): NonEmptyVector[A] =
    create(toVector.sortBy(f)(B.toOrdering))

  def sorted[AA >: A](implicit AA: Order[AA]): NonEmptyVector[AA] =
    create(toVector.sorted(AA.toOrdering))
}

sealed abstract private[data] class NonEmptyVectorInstances  extends NonEmptyVectorInstances1  {

  implicit val catsDataInstancesForNonEmptyVector: Bimonad[NonEmptyVector] with NonEmptyTraverse[NonEmptyVector] =
    new AbstractNonEmptyBimonadTraverse[Vector, NonEmptyVector] {

      def extract[A](fa: NonEmptyVector[A]): A = fa.head

      def nonEmptyTraverse[G[_], A, B](
                                        nel: NonEmptyVector[A]
                                      )(f: A => G[B])(implicit G: Apply[G]): G[NonEmptyVector[B]] =
        Foldable[Vector]
          .reduceRightToOption[A, G[Vector[B]]](nel.tail)(a => G.map(f(a))(_ +: Vector.empty)) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ +: _)
        }
          .map {
            case None => G.map(f(nel.head))(NonEmptyVector(_, Vector.empty))
            case Some(gtail) => G.map2(f(nel.head), gtail)(NonEmptyVector(_, _))
          }
          .value

      def reduceLeftTo[A, B](fa: NonEmptyVector[A])(f: A => B)(g: (B, A) => B): B = fa.reduceLeftTo(f)(g)

      def reduceRightTo[A, B](fa: NonEmptyVector[A])(f: A => B)(g: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
        Eval.defer(fa.reduceRightTo(a => Eval.now(f(a))) { (a, b) =>
          Eval.defer(g(a, b))
        })


      override def toNonEmptyList[A](fa: NonEmptyVector[A]): NonEmptyList[A] =
        fa.toNonEmptyList


      override def nonEmptyPartition[A, B, C](
                                               fa: NonEmptyVector[A]
                                             )(f: (A) => Either[B, C]): Ior[NonEmptyList[B], NonEmptyList[C]] = {
        import cats.syntax.either._

        reduceLeftTo(fa)(a => f(a).bimap(NonEmptyVector.one, NonEmptyVector.one).toIor)(
          (ior, a) =>
            (f(a), ior) match {
              case (Right(c), Ior.Left(_)) => ior.putRight(NonEmptyVector.one(c))
              case (Right(c), _) => ior.map(_ :+ c)
              case (Left(b), Ior.Right(_)) => ior.putLeft(NonEmptyVector.one(b))
              case (Left(b), _) => ior.leftMap(_ :+ b)
            }
        ).bimap(_.toNonEmptyList, _.toNonEmptyList)

      }
    }

  implicit def catsDataParallelForNonEmptyVector[A]: NonEmptyParallel[NonEmptyVector, ZipNonEmptyVector] =
    new NonEmptyParallel[NonEmptyVector, ZipNonEmptyVector] {

      def apply: Apply[ZipNonEmptyVector] = ZipNonEmptyVector.catsDataCommutativeApplyForZipNonEmptyVector
      def flatMap: FlatMap[NonEmptyVector] = catsDataInstancesForNonEmptyVector

      def sequential: ZipNonEmptyVector ~> NonEmptyVector =
        λ[ZipNonEmptyVector ~> NonEmptyVector](_.value)

      def parallel: NonEmptyVector ~> ZipNonEmptyVector =
        λ[NonEmptyVector ~> ZipNonEmptyVector](nev => new ZipNonEmptyVector(nev))
    }

  implicit def catsDataOrderForNonEmptyVector[A: Order]: Order[NonEmptyVector[A]] =
    Order[Vector[A]].asInstanceOf[Order[NonEmptyVector[A]]]

  implicit def catsDataSemigroupForNonEmptyVector[A]: Semigroup[NonEmptyVector[A]] =
    Semigroup[Vector[A]].asInstanceOf[Semigroup[NonEmptyVector[A]]]

  implicit def catsDataShowForNonEmptyVector[A](implicit A: Show[A]): Show[NonEmptyVector[A]] =
    Show.show[NonEmptyVector[A]](_.show)


}

sealed abstract private[data] class NonEmptyVectorInstances1  extends NonEmptyVectorInstances2  {
  implicit val catsDataSemigroupKForNonEmptyVector: SemigroupK[NonEmptyVector] =
    SemigroupK[Vector].asInstanceOf[SemigroupK[NonEmptyVector]]

  implicit def catsDataHashForNonEmptyVector[A: Hash]: Hash[NonEmptyVector[A]] =
    Hash[Vector[A]].asInstanceOf[Hash[NonEmptyVector[A]]]

}

sealed abstract private[data] class NonEmptyVectorInstances2 extends NonEmptyVectorInstances3 {
  implicit def catsDataPartialOrderForNonEmptyVector[A: PartialOrder]: PartialOrder[NonEmptyVector[A]] =
    PartialOrder[Vector[A]].asInstanceOf[PartialOrder[NonEmptyVector[A]]]
}

sealed abstract private[data] class NonEmptyVectorInstances3 {
  implicit def catsDataEqForNonEmptyVector[A: Eq]: Eq[NonEmptyVector[A]] =
    Eq[Vector[A]].asInstanceOf[Eq[NonEmptyVector[A]]]
}