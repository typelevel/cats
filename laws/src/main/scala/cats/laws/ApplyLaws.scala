package cats
package laws

import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.functor._

/**
 * Laws that must be obeyed by any `Apply`.
 */
trait ApplyLaws[F[_]] extends FunctorLaws[F] with SemigroupalLaws[F] {
  implicit override def F: Apply[F]

  def applyComposition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): IsEq[F[C]] = {
    val compose: (B => C) => (A => B) => (A => C) = _.compose
    fbc.ap(fab.ap(fa)) <-> fbc.map(compose).ap(fab).ap(fa)
  }

  def map2ProductConsistency[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map(F.product(fa, fb)) { case (a, b) => f(a, b) } <-> F.map2(fa, fb)(f)

  def map2EvalConsistency[A, B, C](fa: F[A], fb: F[B], f: (A, B) => C): IsEq[F[C]] =
    F.map2(fa, fb)(f) <-> F.map2Eval(fa, Eval.now(fb))(f).value

  def productRConsistency[A, B](fa: F[A], fb: F[B]): IsEq[F[B]] =
    F.productR(fa)(fb) <-> F.map2(fa, fb)((_, b) => b)

  def productLConsistency[A, B](fa: F[A], fb: F[B]): IsEq[F[A]] =
    F.productL(fa)(fb) <-> F.map2(fa, fb)((a, _) => a)

  def selectAssociativity[A, B, C](fa: F[Either[A, B]], fb: F[Either[C, A => B]], fc: F[C => A => B]): IsEq[F[B]] = {
    val fa0 = fa.map(_.map(_.asRight[(C, A)]))
    val fb0 = fb.map { either => (a: A) => either.bimap(c => (c, a), f => f(a)) }
    val fc0 = fc.map(Function.uncurried(_).tupled)
    fa.select(fb.select(fc)) <-> fa0.select(fb0).select(fc0)
  }

  def branchConsistency[A, B, C](fab: F[Either[A, B]], fl: F[A => C], fr: F[B => C]): IsEq[F[C]] = {
    fab.branch(fl)(fr) <-> {
      val lhs = {
        val innerLhs: F[Either[A, Either[B, C]]] = F.map(fab)(_.map(Left(_)))
        val innerRhs: F[A => Either[B, C]] = F.map(fl)(_.andThen(Right(_)))
        F.select(innerLhs)(innerRhs)
      }
      F.select(lhs)(fr)
    }
  }

  def ifSConsistency[A](fb: F[Boolean], ft: F[A], ff: F[A]): IsEq[F[A]] = {
    fb.ifS(ft)(ff) <-> {
      val condition: F[Either[Unit, Unit]] = F.map(fb)(p => if (p) Left(()) else Right(()))
      val left: F[Unit => A] = F.map(ft)(Function.const)
      val right: F[Unit => A] = F.map(ff)(Function.const)
      F.branch(condition)(left)(right)
    }
  }

  def selectAConsistency[A, B](fab: F[Either[A, B]], ff: F[A => B]): IsEq[F[B]] =
    F.selectA(fab)(ff) <-> F.map2(fab, ff) {
      case (Left(a), f)  => f(a)
      case (Right(b), _) => b
    }
}

object ApplyLaws {
  def apply[F[_]](implicit ev: Apply[F]): ApplyLaws[F] =
    new ApplyLaws[F] { def F: Apply[F] = ev }
}
