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
package free

/**
 * Free may be viewed either as a sort of sequential program construction mechanism, wherein
 * programs are assembled and then finally interpreted via foldMap, or it may be viewed as a
 * recursive data structure shaped by the ''pattern'' of its suspension functor. In this view,
 * it is helpful to think of Free as being the following recursive type alias:
 *
 * {{{
 * type Free[S[_], A] = Either[S[Free[S, A]], A]
 * }}}
 *
 * Thus, a Free is a tree of Either(s) in which the data (of type A) is at the leaves, and each
 * branching point is defined by the shape of the S functor. This kind of structural interpretation
 * of Free is very useful in certain contexts, such as when representing expression trees in
 * compilers and interpreters.
 *
 * Using this interpretation, we can define many common instances over the ''data structure'' of
 * Free, most notably Eq and Traverse (Show is also possible, though far less useful). This makes
 * it much more convenient to use Free structures as if they were conventional data structures.
 *
 * Unfortunately, this functionality fundamentally requires recursive implicit resolution. This
 * feature was added to Scala in 2.13 (and retained in Scala 3) in the form of by-name implicits,
 * but is fundamentally unavailable in Scala 2.12 and earlier without using something like Shapeless.
 * For that reason, all of these instances are only available on 2.13 and above.
 */
private trait FreeStructuralInstances extends FreeStructuralInstances0

private trait FreeStructuralInstances0 extends FreeStructuralInstances1 {

  implicit def catsFreeShowForFree[S[_], A](implicit
    SF: Functor[S],
    S: => Show[S[Free[S, A]]],
    A: Show[A]
  ): Show[Free[S, A]] = _.resume match {
    case Right(a)  => A.show(a)
    case Left(sfa) => S.show(sfa)
  }

  implicit def catsFreeHashForFree[S[_], A](implicit
    SF: Functor[S],
    S0: => Hash[S[Free[S, A]]],
    A0: Hash[A]
  ): Hash[Free[S, A]] =
    new FreeStructuralHash[S, A] {
      def functor = SF
      def S = S0
      def A = A0
    }

  trait FreeStructuralHash[S[_], A] extends FreeStructuralEq[S, A] with Hash[Free[S, A]] {
    implicit override def S: Hash[S[Free[S, A]]]
    implicit override def A: Hash[A]

    def hash(fsa: Free[S, A]): Int =
      fsa.resume match {
        case Right(a)  => A.hash(a)
        case Left(sfa) => S.hash(sfa)
      }
  }
}

private trait FreeStructuralInstances1 extends FreeStructuralInstances2 {

  implicit def catsFreePartialOrderForFree[S[_], A](implicit
    SF: Functor[S],
    S0: => PartialOrder[S[Free[S, A]]],
    A0: PartialOrder[A]
  ): PartialOrder[Free[S, A]] =
    new FreeStructuralPartialOrder[S, A] {
      def functor = SF
      def S = S0
      def A = A0
    }

  trait FreeStructuralPartialOrder[S[_], A] extends PartialOrder[Free[S, A]] {
    implicit def functor: Functor[S]
    implicit def S: PartialOrder[S[Free[S, A]]]
    implicit def A: PartialOrder[A]

    def partialCompare(left: Free[S, A], right: Free[S, A]): Double =
      (left.resume, right.resume) match {
        case (Right(leftA), Right(rightA)) =>
          A.partialCompare(leftA, rightA)

        case (Left(leftS), Left(rightS)) =>
          S.partialCompare(leftS, rightS)

        case (Left(_), Right(_)) | (Right(_), Left(_)) =>
          Double.NaN
      }
  }
}

private trait FreeStructuralInstances2 {

  implicit def catsFreeEqForFree[S[_], A](implicit
    SF: Functor[S],
    S0: => Eq[S[Free[S, A]]],
    A0: Eq[A]
  ): Eq[Free[S, A]] =
    new FreeStructuralEq[S, A] {
      def functor = SF
      def S = S0
      def A = A0
    }

  trait FreeStructuralEq[S[_], A] extends Eq[Free[S, A]] {
    implicit def functor: Functor[S]
    implicit def S: Eq[S[Free[S, A]]]
    implicit def A: Eq[A]

    def eqv(left: Free[S, A], right: Free[S, A]): Boolean =
      (left.resume, right.resume) match {
        case (Right(leftA), Right(rightA)) =>
          A.eqv(leftA, rightA)

        case (Left(leftS), Left(rightS)) =>
          S.eqv(leftS, rightS)

        case (Left(_), Right(_)) | (Right(_), Left(_)) =>
          false
      }
  }
}
