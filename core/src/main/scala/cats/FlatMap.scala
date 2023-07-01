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
 * FlatMap type class gives us flatMap, which allows us to have a value
 * in a context (F[A]) and then feed that into a function that takes
 * a normal value and returns a value in a context (A => F[B]).
 *
 * One motivation for separating this out from Monad is that there are
 * situations where we can implement flatMap but not pure.  For example,
 * we can implement map or flatMap that transforms the values of Map[K, *],
 * but we can't implement pure (because we wouldn't know what key to use
 * when instantiating the new Map).
 *
 * @see See [[https://github.com/typelevel/cats/issues/3]] for some discussion.
 *
 * Must obey the laws defined in cats.laws.FlatMapLaws.
 */
trait FlatMap[F[_]] extends Apply[F] with FlatMapArityFunctions[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  /**
   * "flatten" a nested `F` of `F` structure into a single-layer `F` structure.
   *
   * This is also commonly called `join`.
   *
   * Example:
   * {{{
   * scala> import cats.Eval
   * scala> import cats.syntax.all._
   *
   * scala> val nested: Eval[Eval[Int]] = Eval.now(Eval.now(3))
   * scala> val flattened: Eval[Int] = nested.flatten
   * scala> flattened.value
   * res0: Int = 3
   * }}}
   */
  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  /**
   * Sequentially compose two actions, discarding any value produced by the first. This variant of
   * [[productR]] also lets you define the evaluation strategy of the second action. For instance
   * you can evaluate it only ''after'' the first action has finished:
   *
   * {{{
   * scala> import cats.Eval
   * scala> import cats.syntax.all._
   * scala> val fa: Option[Int] = Some(3)
   * scala> def fb: Option[String] = Some("foo")
   * scala> fa.productREval(Eval.later(fb))
   * res0: Option[String] = Some(foo)
   * }}}
   */
  def productREval[A, B](fa: F[A])(fb: Eval[F[B]]): F[B] = flatMap(fa)(_ => fb.value)

  @deprecated("Use productREval instead.", "1.0.0-RC2")
  private[cats] def followedByEval[A, B](fa: F[A])(fb: Eval[F[B]]): F[B] = productREval(fa)(fb)

  /**
   * Sequentially compose two actions, discarding any value produced by the second. This variant of
   * [[productL]] also lets you define the evaluation strategy of the second action. For instance
   * you can evaluate it only ''after'' the first action has finished:
   *
   * {{{
   * scala> import cats.Eval
   * scala> import cats.syntax.all._
   * scala> var count = 0
   * scala> val fa: Option[Int] = Some(3)
   * scala> def fb: Option[Unit] = Some(count += 1)
   * scala> fa.productLEval(Eval.later(fb))
   * res0: Option[Int] = Some(3)
   * scala> assert(count == 1)
   * scala> none[Int].productLEval(Eval.later(fb))
   * res1: Option[Int] = None
   * scala> assert(count == 1)
   * }}}
   */
  def productLEval[A, B](fa: F[A])(fb: Eval[F[B]]): F[A] = flatMap(fa)(a => as(fb.value, a))

  @deprecated("Use productLEval instead.", "1.0.0-RC2")
  private[cats] def forEffectEval[A, B](fa: F[A])(fb: Eval[F[B]]): F[A] = productLEval(fa)(fb)

  override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff)(f => map(fa)(f))

  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    flatMap(fa)(a => map(fb)(b => (a, b)))

  override def ap2[A, B, Z](ff: F[(A, B) => Z])(fa: F[A], fb: F[B]): F[Z] =
    flatMap(fa)(a => flatMap(fb)(b => map(ff)(_(a, b))))

  override def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  override def map2Eval[A, B, Z](fa: F[A], fb: Eval[F[B]])(f: (A, B) => Z): Eval[F[Z]] =
    Eval.now(flatMap(fa)(a => map(fb.value)(b => f(a, b))))

  override def productR[A, B](fa: F[A])(fb: F[B]): F[B] =
    flatMap(fa)(_ => fb)

  override def productL[A, B](fa: F[A])(fb: F[B]): F[A] =
    map2(fa, fb)((a, _) => a)

  /**
   * Pair `A` with the result of function application.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   * scala> List("12", "34", "56").mproduct(_.toList)
   * res0: List[(String, Char)] = List((12,1), (12,2), (34,3), (34,4), (56,5), (56,6))
   * }}}
   */
  def mproduct[A, B](fa: F[A])(f: A => F[B]): F[(A, B)] =
    flatMap(fa)(a => map(f(a))((a, _)))

  /**
   * `if` lifted into monad.
   */

  def ifM[B](fa: F[Boolean])(ifTrue: => F[B], ifFalse: => F[B]): F[B] =
    flatMap(fa)(if (_) ifTrue else ifFalse)

  /**
   * Keeps calling `f` until a `scala.util.Right[B]` is returned.
   *
   * Based on Phil Freeman's
   * [[http://functorial.com/stack-safety-for-free/index.pdf Stack Safety for Free]].
   *
   * Implementations of this method should use constant stack space relative to `f`.
   */
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]

  /**
   * Apply a monadic function and discard the result while keeping the effect.
   *
   * {{{
   * scala> import cats._, implicits._
   * scala> Option(1).flatTap(_ => None)
   * res0: Option[Int] = None
   * scala> Option(1).flatTap(_ => Some("123"))
   * res1: Option[Int] = Some(1)
   * scala> def nCats(n: Int) = List.fill(n)("cat")
   * nCats: (n: Int)List[String]
   * scala> List[Int](0).flatTap(nCats)
   * res2: List[Int] = List()
   * scala> List[Int](4).flatTap(nCats)
   * res3: List[Int] = List(4, 4, 4, 4)
   * }}}
   */
  def flatTap[A, B](fa: F[A])(f: A => F[B]): F[A] =
    flatMap(fa)(a => as(f(a), a))

  /**
   * Like an infinite loop of >> calls. This is most useful effect loops
   * that you want to run forever in for instance a server.
   *
   * This will be an infinite loop, or it will return an F[Nothing].
   *
   * Be careful using this.
   * For instance, a List of length k will produce a list of length k^n at iteration
   * n. This means if k = 0, we return an empty list, if k = 1, we loop forever
   * allocating single element lists, but if we have a k > 1, we will allocate
   * exponentially increasing memory and very quickly OOM.
   */

  def foreverM[A, B](fa: F[A]): F[B] = {
    // allocate two things once for efficiency.
    val leftUnit = Left(())
    val stepResult: F[Either[Unit, B]] = as(fa, leftUnit)
    tailRecM(())(_ => stepResult)
  }

  /**
   * iterateForeverM is almost exclusively useful for effect types. For instance,
   * A may be some state, we may take the current state, run some effect to get
   * a new state and repeat.
   */

  def iterateForeverM[A, B](a: A)(f: A => F[A]): F[B] =
    tailRecM[A, B](a)(f.andThen { fa =>
      map(fa)(Left(_): Either[A, B])
    })

  /**
   * This repeats an F until we get defined values. This can be useful
   * for polling type operations on State (or RNG) Monads, or in effect
   * monads.
   */

  def untilDefinedM[A](foa: F[Option[A]]): F[A] = {
    val leftUnit: Either[Unit, A] = Left(())
    val feither: F[Either[Unit, A]] = map(foa) {
      case None    => leftUnit
      case Some(a) => Right(a)
    }
    tailRecM(())(_ => feither)
  }
}

object FlatMap {

  /**
   * Summon an instance of [[FlatMap]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: FlatMap[F]): FlatMap[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllFlatMapOps[F[_], A](target: F[A])(implicit tc: FlatMap[F]): AllOps[F, A] {
      type TypeClassType = FlatMap[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = FlatMap[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: FlatMap[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def flatMap[B](f: A => F[B]): F[B] = typeClassInstance.flatMap[A, B](self)(f)
    def flatten[B](implicit ev$1: A <:< F[B]): F[B] = typeClassInstance.flatten[B](self.asInstanceOf[F[F[B]]])
    def productREval[B](fb: Eval[F[B]]): F[B] = typeClassInstance.productREval[A, B](self)(fb)
    def productLEval[B](fb: Eval[F[B]]): F[A] = typeClassInstance.productLEval[A, B](self)(fb)
    def mproduct[B](f: A => F[B]): F[(A, B)] = typeClassInstance.mproduct[A, B](self)(f)
    def flatTap[B](f: A => F[B]): F[A] = typeClassInstance.flatTap[A, B](self)(f)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Apply.AllOps[F, A] {
    type TypeClassType <: FlatMap[F]
  }
  trait ToFlatMapOps extends Serializable {
    implicit def toFlatMapOps[F[_], A](target: F[A])(implicit tc: FlatMap[F]): Ops[F, A] {
      type TypeClassType = FlatMap[F]
    } =
      new Ops[F, A] {
        type TypeClassType = FlatMap[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToFlatMapOps

}
