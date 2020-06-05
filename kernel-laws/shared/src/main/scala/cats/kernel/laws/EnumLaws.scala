package cats.kernel
package laws


// trait PartialPreviousLaws[A] extends LowerBoundedLaws[A] {

//   implicit def P: PartialPrevious[A]

//   def previousMinBound: IsEq[Option[A]] =
//     P.partialPrevious(P.minBound) <-> None

//   def partialPreviousLessThan(x: A): IsEq[Boolean] =
//     P.partialPrevious(x).map(E.lt(_, x)).getOrElse(true) <-> true

// }

// object PartialPreviousLaws {
//   def apply[A](implicit ev: PartialPrevious[A]): PartialPreviousLaws[A] =
//     new PartialPreviousLaws[A] {
//       def P: PartialPrevious[A] = ev
//       def B: LowerBounded[A] = ev
//       def E: PartialOrder[A] = ev.partialOrder
//     }
// }

// trait PreviousLaws[A] extends PartialOrderLaws[A] {

//   implicit def P: Previous[A]

//   def previousLessThan(x: A): IsEq[Boolean] =
//     E.lt(P.previous(x) , x) <-> true
// }

// trait PartialNextLaws[A] extends PartialOrderLaws[A] {

//   implicit def N: PartialNext[A]

//   // def maximum: IsEq[Option[A]] =
//   //   N.partialNext(N.maxBound) <-> None

//   def nextWeak(x: A): IsEq[Boolean] =
//     N.partialNext(x).map(E.gt(_, x)).getOrElse(true) <-> true

//   def nextStrong(a: A, b: A): IsEq[Boolean] =
//     (if(E.lt(a, b)) N.partialNext(a).map(E.lteqv(_, b)).getOrElse(true) else true) <-> true

// }


// object PartialNextLaws {
//   def apply[A](implicit ev: PartialNext[A]): PartialNextLaws[A] =
//     new PartialNextLaws[A] {
//       def N: PartialNext[A] = ev
//       def B: UpperBounded[A] = ev
//       def E: PartialOrder[A] = ev.partialOrder
//     }
// }

// trait NextLaws[A] extends PartialOrderLaws[A] {

//   implicit def N: Next[A]

//   def nextGreaterThan(x: A): IsEq[Boolean] =
//     E.gt(N.next(x) , x) <-> true
// }
