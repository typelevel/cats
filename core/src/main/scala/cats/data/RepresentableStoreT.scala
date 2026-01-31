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

package cats.data

import cats.{Applicative, Apply, Comonad, Functor, Monoid, Representable}

/*
 * The dual of `StateT`. Stores some state `A` indexed by
 * a type `S` with the notion of a cursor tracking the
 * current position in the index.
 *
 * This state can be extracted if the underlying `F` has
 * a Comonad instance.
 *
 * This is the (co)monad-transformer version of `RepresentableStore`
 */
final case class RepresentableStoreT[W[_], F[_], S, A](runF: W[F[A]], index: S)(implicit F: Representable.Aux[F, S]) {

  def run(implicit W: Functor[W]): W[A] = W.map(runF)(fa => F.index(fa)(index))

  /**
   * Peek at what the focus would be for a given focus s.
   */
  def peek(s: S)(implicit W: Comonad[W]): A = W.extract(W.map(runF)(fa => F.index(fa)(s)))

  /**
   * Peek at what the focus would be if the current focus where transformed
   * with the given function.
   */
  def peeks(f: S => S)(implicit W: Comonad[W]): A = peek(f(index))

  /**
   * Set the current focus.
   */
  def seek(s: S): RepresentableStoreT[W, F, S, A] = RepresentableStoreT(runF, s)

  /**
   * Modify the current focus with the given function.
   */
  def seeks(f: S => S): RepresentableStoreT[W, F, S, A] = seek(f(index))

  /**
   * Extract the focus at the current index.
   */
  def extract(implicit W: Comonad[W]): A = peek(index)

  /**
   * `coflatMap` is the dual of `flatMap` on `FlatMap`. It applies
   * a value in a context to a function that takes a value
   * in a context and returns a normal value.
   */
  def coflatMap[B](f: RepresentableStoreT[W, F, S, A] => B)(implicit W: Comonad[W]): RepresentableStoreT[W, F, S, B] =
    RepresentableStoreT(
      W.map(W.coflatten(runF))((x: W[F[A]]) => F.tabulate(s => f(RepresentableStoreT(x, s)))),
      index
    )

  /**
   * `coflatten` is the dual of `flatten` on `FlatMap`. Whereas flatten removes
   * a layer of `F`, coflatten adds a layer of `F`
   */
  def coflatten(implicit W: Comonad[W]): RepresentableStoreT[W, F, S, RepresentableStoreT[W, F, S, A]] =
    RepresentableStoreT(
      W.map(W.coflatten(runF))((x: W[F[A]]) => F.tabulate(s => RepresentableStoreT(x, s))),
      index
    )

  /**
   * Functor `map` for StoreT
   */
  def map[B](f: A => B)(implicit W: Functor[W]): RepresentableStoreT[W, F, S, B] = RepresentableStoreT(
    W.map(runF)((fa: F[A]) => F.F.map(fa)(f)),
    index
  )

  /**
   * Given a functorial computation on the index `S` peek at the value in that functor.
   */
  def experiment[G[_]](f: S => G[S])(implicit W: Comonad[W], G: Functor[G]): G[A] =
    G.map(f(index))(peek(_))

}

object RepresentableStoreT extends RepresentableStoreTInstances1 {

  def pure[W[_], F[_], S, A](
    x: A
  )(implicit W: Applicative[W], F: Representable.Aux[F, S], S: Monoid[S]): RepresentableStoreT[W, F, S, A] =
    RepresentableStoreT(W.pure(F.tabulate((_: S) => x)), S.empty)

  implicit def comonadForStoreT[W[_]: Comonad, F[_], S]: Comonad[RepresentableStoreT[W, F, S, *]] =
    new Comonad[RepresentableStoreT[W, F, S, *]] {

      override def map[A, B](fa: RepresentableStoreT[W, F, S, A])(f: A => B): RepresentableStoreT[W, F, S, B] =
        fa.map(f)

      override def coflatMap[A, B](fa: RepresentableStoreT[W, F, S, A])(
        f: RepresentableStoreT[W, F, S, A] => B
      ): RepresentableStoreT[W, F, S, B] =
        fa.coflatMap(f)

      override def extract[A](fa: RepresentableStoreT[W, F, S, A]): A = fa.extract

    }
}

trait RepresentableStoreTInstances1 extends RepresentableStoreTInstances2 {

  implicit def applicativeForStoreT[W[_], F[_], S](implicit
    W: Applicative[W],
    F: Representable.Aux[F, S],
    S: Monoid[S]
  ): Applicative[RepresentableStoreT[W, F, S, *]] =
    new Apply.AbstractApply[RepresentableStoreT[W, F, S, *]] with Applicative[RepresentableStoreT[W, F, S, *]] {

      def pure[A](x: A): RepresentableStoreT[W, F, S, A] = RepresentableStoreT.pure[W, F, S, A](x)

      def ap[A, B](
        ff: RepresentableStoreT[W, F, S, A => B]
      )(fa: RepresentableStoreT[W, F, S, A]): RepresentableStoreT[W, F, S, B] = RepresentableStoreT(
        W.map(W.tuple2(ff.runF, fa.runF)) { case (f, a) =>
          F.tabulate((s: S) => F.index(f)(s).apply(F.index(a)(s)))
        },
        S.combine(ff.index, fa.index)
      )

    }
}

trait RepresentableStoreTInstances2 {

  implicit def functorForStoreT[W[_]: Functor, F[_], S]: Functor[RepresentableStoreT[W, F, S, *]] =
    new Functor[RepresentableStoreT[W, F, S, *]] {

      override def map[A, B](fa: RepresentableStoreT[W, F, S, A])(f: A => B): RepresentableStoreT[W, F, S, B] =
        fa.map(f)

    }
}
