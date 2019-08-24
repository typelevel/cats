package cats.kernel
package instances

import scala.collection.immutable.Seq
import scala.annotation.tailrec

trait SeqInstances extends SeqInstances1 {
  implicit def catsKernelStdOrderForSeq[A: Order]: Order[Seq[A]] =
    new SeqOrder[A]

  implicit def catsKernelStdMonoidForSeq[A]: Monoid[Seq[A]] =
    new SeqMonoid[A]
}

trait SeqInstances1 extends SeqInstances2 {
  implicit def catsKernelStdPartialOrderForSeq[A: PartialOrder]: PartialOrder[Seq[A]] =
    new SeqPartialOrder[A]

  implicit def catsKernelStdHashForSeq[A: Hash]: Hash[Seq[A]] =
    new SeqHash[A]
}

trait SeqInstances2 {
  implicit def catsKernelStdEqForSeq[A: Eq]: Eq[Seq[A]] =
    new SeqEq[A]
}

class SeqOrder[A](implicit ev: Order[A]) extends Order[Seq[A]] {
  def compare(xs: Seq[A], ys: Seq[A]): Int = {
    @tailrec def loop(xs: Seq[A], ys: Seq[A]): Int =
      xs match {
        case Seq() =>
          if (ys.isEmpty) 0 else -1
        case x +: xs1 =>
          ys match {
            case Seq() => 1
            case y +: ys1 =>
              val n = ev.compare(x, y)
              if (n != 0) n else loop(xs1, ys1)
          }
      }
    if (xs eq ys) 0 else loop(xs, ys)
  }
}

class SeqPartialOrder[A](implicit ev: PartialOrder[A]) extends PartialOrder[Seq[A]] {
  def partialCompare(xs: Seq[A], ys: Seq[A]): Double = {
    @tailrec def loop(xs: Seq[A], ys: Seq[A]): Double =
      xs match {
        case Seq() =>
          if (ys.isEmpty) 0.0 else -1.0
        case x +: xs1 =>
          ys match {
            case Seq() => 1.0
            case y +: ys1 =>
              val n = ev.partialCompare(x, y)
              if (n != 0.0) n else loop(xs1, ys1)
          }
      }
    if (xs eq ys) 0.0 else loop(xs, ys)
  }
}

class SeqHash[A](implicit ev: Hash[A]) extends SeqEq[A]()(ev) with Hash[Seq[A]] {
  def hash(x: Seq[A]): Int = StaticMethods.seqHash(x)(ev)
}

class SeqEq[A](implicit ev: Eq[A]) extends Eq[Seq[A]] {
  def eqv(xs: Seq[A], ys: Seq[A]): Boolean = {
    def loop(xs: Seq[A], ys: Seq[A]): Boolean =
      xs match {
        case Seq() =>
          ys.isEmpty
        case x +: xs1 =>
          ys match {
            case y +: ys1 =>
              if (ev.eqv(x, y)) loop(xs1, ys1) else false
            case Seq() =>
              false
          }
      }
    (xs eq ys) || loop(xs, ys)
  }
}

class SeqMonoid[A] extends Monoid[Seq[A]] {
  def empty: Seq[A] = Seq.empty
  def combine(x: Seq[A], y: Seq[A]): Seq[A] = x ++ y

  override def combineN(x: Seq[A], n: Int): Seq[A] =
    StaticMethods.combineNIterable(Seq.newBuilder[A], x, n)

  override def combineAll(xs: TraversableOnce[Seq[A]]): Seq[A] =
    StaticMethods.combineAllIterable(Seq.newBuilder[A], xs)
}
