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
 * Functor.
 *
 * The name is short for "covariant functor".
 *
 * Must obey the laws defined in cats.laws.FunctorLaws.
 */
trait Functor[F[_]] extends Invariant[F] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = map(fa)(f)

  // derived methods

  /**
   * Alias for [[map]], since [[map]] can't be injected as syntax if
   * the implementing type already had a built-in `.map` method.
   *
   * Example:
   * {{{
   * scala> import cats.syntax.all._
   *
   * scala> val m: Map[Int, String] = Map(1 -> "hi", 2 -> "there", 3 -> "you")
   *
   * scala> m.fmap(_ ++ "!")
   * res0: Map[Int,String] = Map(1 -> hi!, 2 -> there!, 3 -> you!)
   * }}}
   */
  final def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)

  /**
   * Lifts natural subtyping covariance of covariant Functors.
   *
   * NOTE: In certain (perhaps contrived) situations that rely on universal
   * equality this can result in a `ClassCastException`, because it is
   * implemented as a type cast. It could be implemented as `map(identity)`, but
   * according to the functor laws, that should be equal to `fa`, and a type
   * cast is often much more performant.
   * See [[https://github.com/typelevel/cats/issues/1080#issuecomment-225892635 this example]]
   * of `widen` creating a `ClassCastException`.
   *
   * Example:
   * {{{
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForOption
   *
   * scala> val s = Some(42)
   * scala> Functor[Option].widen(s)
   * res0: Option[Int] = Some(42)
   * }}}
   */
  def widen[A, B >: A](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]

  /**
   * Lift a function f to operate on Functors
   *
   * Example:
   * {{{
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForOption
   *
   * scala> val o = Option(42)
   * scala> Functor[Option].lift((x: Int) => x + 10)(o)
   * res0: Option[Int] = Some(52)
   * }}}
   */
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  /**
   * Empty the fa of the values, preserving the structure
   *
   * Example:
   * {{{
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForList
   *
   * scala> Functor[List].void(List(1,2,3))
   * res0: List[Unit] = List((), (), ())
   * }}}
   */
  def void[A](fa: F[A]): F[Unit] = as(fa, ())

  /**
   * Tuple the values in fa with the result of applying a function
   * with the value
   *
   * Example:
   * {{{
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForOption
   *
   * scala> Functor[Option].fproduct(Option(42))(_.toString)
   * res0: Option[(Int, String)] = Some((42,42))
   * }}}
   */
  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => a -> f(a))

  /**
   *  Pair the result of function application with `A`.
   *
   * Example:
   * {{{
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForOption
   *
   * scala> Functor[Option].fproductLeft(Option(42))(_.toString)
   * res0: Option[(String, Int)] = Some((42,42))
   * }}}
   */
  def fproductLeft[A, B](fa: F[A])(f: A => B): F[(B, A)] = map(fa)(a => f(a) -> a)

  /**
   * Replaces the `A` value in `F[A]` with the supplied value.
   *
   * Example:
   *
   * {{{
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForList
   *
   * scala> Functor[List].as(List(1,2,3), "hello")
   * res0: List[String] = List(hello, hello, hello)
   * }}}
   */
  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)

  /**
   * Tuples the `A` value in `F[A]` with the supplied `B` value, with the `B` value on the left.
   *
   * Example:
   * {{{
   * scala> import scala.collection.immutable.Queue
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForQueue
   *
   * scala> Functor[Queue].tupleLeft(Queue("hello", "world"), 42)
   * res0: scala.collection.immutable.Queue[(Int, String)] = Queue((42,hello), (42,world))
   * }}}
   */
  def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)] = map(fa)(a => (b, a))

  /**
   * Tuples the `A` value in `F[A]` with the supplied `B` value, with the `B` value on the right.
   *
   * Example:
   * {{{
   * scala> import scala.collection.immutable.Queue
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForQueue
   *
   * scala> Functor[Queue].tupleRight(Queue("hello", "world"), 42)
   * res0: scala.collection.immutable.Queue[(String, Int)] = Queue((hello,42), (world,42))
   * }}}
   */
  def tupleRight[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(a => (a, b))

  /**
   * Un-zips an `F[(A, B)]` consisting of element pairs or Tuple2 into two separate F's tupled.
   *
   * NOTE: Check for effect duplication, possibly memoize before
   *
   * {{{
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForList
   *
   * scala> Functor[List].unzip(List((1,2), (3, 4)))
   * res0: (List[Int], List[Int]) = (List(1, 3),List(2, 4))
   * }}}
   */

  def unzip[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  /**
   * Lifts `if` to Functor
   *
   * Example:
   * {{{
   * scala> import cats.Functor
   * scala> import cats.implicits.catsStdInstancesForList
   *
   * scala> Functor[List].ifF(List(true, false, false))(1, 0)
   * res0: List[Int] = List(1, 0, 0)
   * }}}
   */

  def ifF[A](fb: F[Boolean])(ifTrue: => A, ifFalse: => A): F[A] = map(fb)(x => if (x) ifTrue else ifFalse)

  def compose[G[_]: Functor]: Functor[λ[α => F[G[α]]]] =
    new ComposedFunctor[F, G] {
      val F = self
      val G = Functor[G]
    }

  def composeBifunctor[G[_, _]: Bifunctor]: Bifunctor[λ[(α, β) => F[G[α, β]]]] =
    new ComposedFunctorBifunctor[F, G] {
      val F = self
      val G = Bifunctor[G]
    }

  override def composeContravariant[G[_]: Contravariant]: Contravariant[λ[α => F[G[α]]]] =
    new ComposedCovariantContravariant[F, G] {
      val F = self
      val G = Contravariant[G]
    }
}

object Functor {

  /**
   * Summon an instance of [[Functor]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: Functor[F]): Functor[F] = instance

  @deprecated("Use cats.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]): AllOps[F, A] {
      type TypeClassType = Functor[F]
    } =
      new AllOps[F, A] {
        type TypeClassType = Functor[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: Functor[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def map[B](f: A => B): F[B] = typeClassInstance.map[A, B](self)(f)
    final def fmap[B](f: A => B): F[B] = typeClassInstance.fmap[A, B](self)(f)
    def widen[B >: A]: F[B] = typeClassInstance.widen[A, B](self)
    def void: F[Unit] = typeClassInstance.void[A](self)
    def fproduct[B](f: A => B): F[(A, B)] = typeClassInstance.fproduct[A, B](self)(f)
    def fproductLeft[B](f: A => B): F[(B, A)] = typeClassInstance.fproductLeft[A, B](self)(f)
    def as[B](b: B): F[B] = typeClassInstance.as[A, B](self, b)
    def tupleLeft[B](b: B): F[(B, A)] = typeClassInstance.tupleLeft[A, B](self, b)
    def tupleRight[B](b: B): F[(A, B)] = typeClassInstance.tupleRight[A, B](self, b)
  }
  trait AllOps[F[_], A] extends Ops[F, A] with Invariant.AllOps[F, A] {
    type TypeClassType <: Functor[F]
  }
  trait ToFunctorOps extends Serializable {
    implicit def toFunctorOps[F[_], A](target: F[A])(implicit tc: Functor[F]): Ops[F, A] {
      type TypeClassType = Functor[F]
    } =
      new Ops[F, A] {
        type TypeClassType = Functor[F]
        val self: F[A] = target
        val typeClassInstance: TypeClassType = tc
      }
  }
  @deprecated("Use cats.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToFunctorOps

}
