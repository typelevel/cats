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

/**
 * Monad.
 *
 * Allows composition of dependent effectful functions.
 *
 * See: [[http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf Monads for functional programming]]
 *
 * Must obey the laws defined in cats.laws.MonadLaws.
 */
trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => pure(f(a)))

  /**
   * Execute an action repeatedly as long as the given `Boolean` expression
   * returns `true`. The condition is evaluated before the loop body.
   * Collects the results into an arbitrary `Alternative` value, such as a `Vector`.
   * This implementation uses append on each evaluation result,
   * so avoid data structures with non-constant append performance, e.g. `List`.
   */

  def whileM[G[_], A](p: F[Boolean])(body: => F[A])(implicit G: Alternative[G]): F[G[A]] = {
    val b = Eval.later(body)
    tailRecM[G[A], G[A]](G.empty)(xs =>
      ifM(p)(
        ifTrue = {
          map(b.value) { bv =>
            Left(G.appendK(xs, bv))
          }
        },
        ifFalse = pure(Right(xs))
      )
    )
  }

  /**
   * Execute an action repeatedly as long as the given `Boolean` expression
   * returns `true`. The condition is evaluated before the loop body.
   * Discards results.
   */

  def whileM_[A](p: F[Boolean])(body: => F[A]): F[Unit] = {
    val continue: Either[Unit, Unit] = Left(())
    val stop: F[Either[Unit, Unit]] = pure(Right(()))
    val b = Eval.later(body)
    tailRecM(())(_ =>
      ifM(p)(
        ifTrue = as(b.value, continue),
        ifFalse = stop
      )
    )
  }

  /**
   * Execute an action repeatedly until the `Boolean` condition returns `true`.
   * The condition is evaluated after the loop body. Collects results into an
   * arbitrary `Alternative` value, such as a `Vector`.
   * This implementation uses append on each evaluation result,
   * so avoid data structures with non-constant append performance, e.g. `List`.
   */
  def untilM[G[_], A](f: F[A])(cond: => F[Boolean])(implicit G: Alternative[G]): F[G[A]] = {
    val p = Eval.later(cond)
    flatMap(f)(x => map(whileM(map(p.value)(!_))(f))(xs => G.prependK(x, xs)))
  }

  /**
   * Execute an action repeatedly until the `Boolean` condition returns `true`.
   * The condition is evaluated after the loop body. Discards results.
   */
  def untilM_[A](f: F[A])(cond: => F[Boolean]): F[Unit] = {
    val p = Eval.later(cond)
    flatMap(f)(_ => whileM_(map(p.value)(!_))(f))
  }

  /**
   * Execute an action repeatedly until its result fails to satisfy the given predicate
   * and return that result, discarding all others.
   */
  def iterateWhile[A](f: F[A])(p: A => Boolean): F[A] =
    flatMap(f) { i =>
      iterateWhileM(i)(_ => f)(p)
    }

  /**
   * Execute an action repeatedly until its result satisfies the given predicate
   * and return that result, discarding all others.
   */
  def iterateUntil[A](f: F[A])(p: A => Boolean): F[A] =
    flatMap(f) { i =>
      iterateUntilM(i)(_ => f)(p)
    }

  /**
   * Apply a monadic function iteratively until its result fails
   * to satisfy the given predicate and return that result.
   */
  def iterateWhileM[A](init: A)(f: A => F[A])(p: A => Boolean): F[A] =
    tailRecM(init) { a =>
      if (p(a))
        map(f(a))(Left(_))
      else
        pure(Right(a))
    }

  /**
   * Apply a monadic function iteratively until its result satisfies
   * the given predicate and return that result.
   */
  def iterateUntilM[A](init: A)(f: A => F[A])(p: A => Boolean): F[A] =
    iterateWhileM(init)(f)(!p(_))

  /**
   * Simulates an if/else-if/else in the context of an F. It evaluates conditions until
   * one evaluates to true, and returns the associated F[A]. If no condition is true,
   * returns els.
   *
   * {{{
   * scala> import cats._
   * scala> Monad[Eval].ifElseM(Eval.later(false) -> Eval.later(1), Eval.later(true) -> Eval.later(2))(Eval.later(5)).value
   * res0: Int = 2
   * }}}
   *
   * Based on a [[https://gist.github.com/1b92a6e338f4e1537692e748c54b9743 gist]] by Daniel Spiewak with a stack-safe
   * [[https://github.com/typelevel/cats/pull/3553#discussion_r468121480 implementation]] due to P. Oscar Boykin
   * @see See [[https://gitter.im/typelevel/cats-effect?at=5f297e4314c413356f56d230]] for the discussion.
   */

  def ifElseM[A](branches: (F[Boolean], F[A])*)(els: F[A]): F[A] = {
    type Branches = List[(F[Boolean], F[A])]

    def step(branches: Branches): F[Either[Branches, A]] =
      branches match {
        case (cond, conseq) :: tail =>
          flatMap(cond) { b => if (b) map(conseq)(Right(_)) else pure(Left(tail)) }
        case Nil =>
          map(els)(Right(_))
      }

    tailRecM(branches.toList)(step)
  }

  /**
   * Modifies the `A` value in `F[A]` with the supplied function, if the function is defined for the value.
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> List(1, 2, 3).flatMapOrKeep{ case 2 => List(2, 22, 222) }
   * res0: List[Int] = List(1, 2, 22, 222, 3)
   * }}}
   */
  def flatMapOrKeep[A, A1 >: A](fa: F[A])(pfa: PartialFunction[A, F[A1]]): F[A1] =
    flatMap(fa)(a => pfa.applyOrElse(a, pure[A1]))

}

object Monad {

  /**
   * Summon an instance of [[Monad]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Monad[F]): Monad[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllMonadOps[F[_], A](target: F[A])(implicit tc: Monad[F]): AllOps[F, A] {
      type TypeClassType = Monad[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Monad[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Monad[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def untilM[G[_]](cond: => F[Boolean])(implicit G: Alternative[G]): F[G[A]] =
      typeClassInstance.untilM[G, A](self)(cond)(G)
    def untilM_(cond: => F[Boolean]): F[Unit] = typeClassInstance.untilM_[A](self)(cond)
    def iterateWhile(p: A => Boolean): F[A] = typeClassInstance.iterateWhile[A](self)(p)
    def iterateUntil(p: A => Boolean): F[A] = typeClassInstance.iterateUntil[A](self)(p)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with FlatMap.AllOps[F, A] with Applicative.AllOps[F, A] {
    type TypeClassType <: Monad[F]
  }
  trait ToMonadOps extends Serializable {
    implicit def toMonadOps[F[_], A](target: F[A])(implicit tc: Monad[F]): Ops[F, A] {
      type TypeClassType = Monad[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Monad[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToMonadOps

}
