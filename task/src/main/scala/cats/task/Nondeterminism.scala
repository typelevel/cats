package cats
package task

import simulacrum.typeclass

import cats.data.Xor
import cats.data.{Stream, StreamT}
import cats.syntax.all._

import Xor.{Left, Right}

/**
 * Nondeterministic monad.
 */
@typeclass(excludeParents=List("NondeterminismFunctions"))
trait Nondeterminism[F[_]] extends Monad[F] with NondeterminismFunctions[F] {

  type PF[-A, +B] = PartialFunction[A, B]

  def arrange[A](fas: List[F[A]]): StreamT[F, A]

  def combineAll[A: Monoid](fas: List[F[A]]): F[A] =
    arrange(fas).foldLeft(Monoid[A].empty)(_ |+| _)(this)

  def gatherPos[A](fas: List[F[A]]): StreamT[F, (A, Int)] =
    arrange(fas.zipWithIndex.map {
      case (fa, i) => map(fa)(a => (a, i))
    })

  def unorderedGather[A](fas: List[F[A]]): F[List[A]] =
    arrange(fas).toList(this)

  def orderedGather[A](fas: List[F[A]]): F[List[A]] =
    map(gatherPos(fas).toList(this))(_.sortBy(_._2).map(_._1))

  def choose[A, B](fa: F[A], fb: F[B]): F[Xor[(A, F[B]), (B, F[A])]] = {
    def coerce[C](s: StreamT[F, Xor[A, B]])(f: PF[Xor[A, B], C]): F[C] =
      map(s.uncons(this))(_.fold(sys.error("!!")) { case (axb, _) => f(axb) })
    val fda = map(fa)(Xor.left[A, B])
    val fdb = map(fb)(Xor.right[A, B])
    map(arrange(fda :: fdb :: Nil).uncons(this)) {
      case Some((Left(a), s)) =>
        Left((a, coerce(s)({ case Right(b) => b })))
      case Some((Right(b), s)) =>
        Right((b, coerce(s)({ case Left(a) => a })))
      case None =>
        sys.error("!!")
    }
  }
}

trait NondeterminismFunctions[F[_]] { self: Nondeterminism[F] =>

  def asyncMap2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] = {
    val lst = fa.asInstanceOf[F[Any]] :: fb.asInstanceOf[F[Any]] :: Nil
    map(orderedGather(lst)) {
      case a :: b :: Nil => f(a.asInstanceOf[A], b.asInstanceOf[B])
      case _ => sys.error("!!")
    }
  }

  def asyncMap3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] = {
    val lst = fa.asInstanceOf[F[Any]] :: fb.asInstanceOf[F[Any]] :: fc.asInstanceOf[F[Any]] :: Nil
    map(orderedGather(lst)) {
      case a :: b :: c :: Nil => f(a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C])
      case _ => sys.error("!!")
    }
  }

  def asyncMap4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] = {
    val lst = fa.asInstanceOf[F[Any]] :: fb.asInstanceOf[F[Any]] :: fc.asInstanceOf[F[Any]] :: fd.asInstanceOf[F[Any]] :: Nil
    map(orderedGather(lst)) {
      case a :: b :: c :: d :: Nil => f(a.asInstanceOf[A], b.asInstanceOf[B], c.asInstanceOf[C], d.asInstanceOf[D])
      case _ => sys.error("!!")
    }
  }

  def foldFirst2[A, B, Z](fa: F[A], fb: F[B])(f0: A => Z, f1: B => Z): F[Z] = {
    val lst = fa.asInstanceOf[F[Any]] :: fb.asInstanceOf[F[Any]] :: Nil
    map(gatherPos(lst).uncons(this)) {
      case Some(((a, 0), _)) => f0(a.asInstanceOf[A])
      case Some(((b, 1), _)) => f1(b.asInstanceOf[B])
      case _ => sys.error("!!")
    }
  }

  def foldFirst3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f0: A => Z, f1: B => Z, f2: C => Z): F[Z] = {
    val lst = fa.asInstanceOf[F[Any]] :: fb.asInstanceOf[F[Any]] :: fc.asInstanceOf[F[Any]] :: Nil
    map(gatherPos(lst).uncons(this)) {
      case Some(((a, 0), _)) => f0(a.asInstanceOf[A])
      case Some(((b, 1), _)) => f1(b.asInstanceOf[B])
      case Some(((c, 2), _)) => f2(c.asInstanceOf[C])
      case _ => sys.error("!!")
    }
  }

  def foldFirst4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f0: A => Z, f1: B => Z, f2: C => Z, f3: D => Z): F[Z] = {
    val lst = fa.asInstanceOf[F[Any]] :: fb.asInstanceOf[F[Any]] :: fc.asInstanceOf[F[Any]] :: fd.asInstanceOf[F[Any]] :: Nil
    map(gatherPos(lst).uncons(this)) {
      case Some(((a, 0), _)) => f0(a.asInstanceOf[A])
      case Some(((b, 1), _)) => f1(b.asInstanceOf[B])
      case Some(((c, 2), _)) => f2(c.asInstanceOf[C])
      case Some(((d, 3), _)) => f3(d.asInstanceOf[D])
      case _ => sys.error("!!")
    }
  }
}
