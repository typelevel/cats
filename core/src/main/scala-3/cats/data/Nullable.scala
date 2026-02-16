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

import cats.{Applicative, Eq, Eval, Hash, Monad, Monoid, Order, PartialOrder, Semigroup, Show, Traverse}
import scala.language.strictEquality
import scala.quoted.*

opaque type Nullable[+A] = A | Null

object Nullable extends NullableInstances {
  // If we enable explicit nulls, strict equality needs this `CanEqual` for `== null` checks.
  // We keep it `private[data]` (not `private`) because explicit nulls are not enabled right now,
  // and a `private given` would otherwise trigger an unused-private-member warning.
  private[data] given [A]: CanEqual[A | Null, Null] = CanEqual.derived

  extension [A](inline nullable: Nullable[A]) {
    inline def fold[B](inline ifNull: => B)(inline f: A => B): B = {
      ${ foldImpl('nullable, 'ifNull, 'f) }
    }

    inline def isNull: Boolean = {
      val value: A | Null = nullable
      value == null
    }

    inline def nonNull: Boolean = {
      !isNull
    }

    inline def map[B](inline f: A => B): Nullable[B] = {
      fold(null: Null)(f)
    }

    inline def orNull: A | Null = {
      nullable
    }

    inline def toOption: Option[A] = {
      fold(None)(Some(_))
    }

    inline def iterator: Iterator[A] = {
      fold(Iterator.empty)(Iterator.single(_))
    }
  }

  inline def apply[A](inline a: A | Null): Nullable[A] = {
    a
  }

  def fromOption[A](opt: Option[A]): Nullable[A] = {
    opt match {
      case Some(a) => a
      case None    => null
    }
  }

  inline def empty[A]: Nullable[A] = {
    null
  }

  // Non-inline typeclass methods in this same file cannot call `n.toOption` / `n.fold`
  // because `fold` is a macro defined in the same source file.
  private inline def nonMacroToOption[A](n: Nullable[A]): Option[A] = {
    val value: A | Null = n
    if value == null then {
      None
    } else {
      Some(value.asInstanceOf[A])
    }
  }

  private[data] def toOptionNullable[A](nullable: Nullable[A]): Option[Nullable[A]] = {
    val value: A | Null = nullable
    if value == null then {
      None
    } else {
      Some(nullable)
    }
  }

  private[data] def eqvNullable[A](na: Nullable[A], nb: Nullable[A])(using A: Eq[A]): Boolean = {
    val a: A | Null = na
    val b: A | Null = nb
    if a == null then {
      b == null
    } else if b == null then {
      false
    } else {
      A.eqv(a.asInstanceOf[A], b.asInstanceOf[A])
    }
  }

  private[data] def hashNullable[A](nullable: Nullable[A])(using A: Hash[A]): Int = {
    val value: A | Null = nullable
    if value == null then {
      None.hashCode()
    } else {
      A.hash(value.asInstanceOf[A])
    }
  }

  private[data] def partialCompareNullable[A](na: Nullable[A], nb: Nullable[A])(using A: PartialOrder[A]): Double = {
    val a: A | Null = na
    val b: A | Null = nb
    if a == null then {
      if b == null then {
        0.0
      } else {
        -1.0
      }
    } else if b == null then {
      1.0
    } else {
      A.partialCompare(a.asInstanceOf[A], b.asInstanceOf[A])
    }
  }

  private[data] def compareNullable[A](na: Nullable[A], nb: Nullable[A])(using A: Order[A]): Int = {
    val a: A | Null = na
    val b: A | Null = nb
    if a == null then {
      if b == null then {
        0
      } else {
        -1
      }
    } else if b == null then {
      1
    } else {
      A.compare(a.asInstanceOf[A], b.asInstanceOf[A])
    }
  }

  private def combineNullable[A](nx: Nullable[A], ny: Nullable[A])(using A: Semigroup[A]): Nullable[A] = {
    val x: A | Null = nx
    val y: A | Null = ny
    if x == null then {
      y
    } else if y == null then {
      x
    } else {
      A.combine(x.asInstanceOf[A], y.asInstanceOf[A])
    }
  }

  given [A](using A: Semigroup[A]): Monoid[Nullable[A]] with {
    def empty: Nullable[A] = {
      Nullable.empty
    }

    def combine(nx: Nullable[A], ny: Nullable[A]): Nullable[A] = {
      combineNullable(nx, ny)
    }
  }

  given [A](using A: Show[A]): Show[Nullable[A]] with {
    def show(nullable: Nullable[A]): String = {
      val value: A | Null = nullable
      if value == null then {
        java.lang.String.valueOf(value.asInstanceOf[AnyRef]).asInstanceOf[String]
      } else {
        A.show(value.asInstanceOf[A])
      }
    }
  }

  /*
   * These methods intentionally use direct null checks or `nonMacroToOption` instead of `fa.fold(...)`.
   * `fold` is implemented as an inline macro in this same source file, and Scala 3
   * rejects calls from non-inline methods to a macro defined in the same file.
   * (`Traverse` methods cannot be inline overrides here.)
   */
  given catsDataTraverseForNullable: Traverse[Nullable] with {
    private[this] val nullableUnit: Nullable[Unit] = ()
    private[this] val nullPair: (Null, Null) = (null, null)

    override def map[A, B](fa: Nullable[A])(f: A => B): Nullable[B] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        f(value.asInstanceOf[A])
      }
    }

    override def as[A, B](fa: Nullable[A], b: B): Nullable[B] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        b
      }
    }

    override def void[A](fa: Nullable[A]): Nullable[Unit] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        nullableUnit
      }
    }

    override def tupleLeft[A, B](fa: Nullable[A], b: B): Nullable[(B, A)] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        (b, value.asInstanceOf[A])
      }
    }

    override def tupleRight[A, B](fa: Nullable[A], b: B): Nullable[(A, B)] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        (value.asInstanceOf[A], b)
      }
    }

    override def fproduct[A, B](fa: Nullable[A])(f: A => B): Nullable[(A, B)] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        val a = value.asInstanceOf[A]
        (a, f(a))
      }
    }

    override def fproductLeft[A, B](fa: Nullable[A])(f: A => B): Nullable[(B, A)] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        val a = value.asInstanceOf[A]
        (f(a), a)
      }
    }

    override def ifF[A](fb: Nullable[Boolean])(ifTrue: => A, ifFalse: => A): Nullable[A] = {
      val value: Boolean | Null = fb
      if value == null then {
        Nullable.empty
      } else if value.asInstanceOf[Boolean] then {
        ifTrue
      } else {
        ifFalse
      }
    }

    override def unzip[A, B](fab: Nullable[(A, B)]): (Nullable[A], Nullable[B]) = {
      val value: (A, B) | Null = fab
      if value == null then {
        nullPair
      } else {
        value.asInstanceOf[(A, B)]
      }
    }

    def foldLeft[A, B](fa: Nullable[A], b: B)(f: (B, A) => B): B = {
      val value: A | Null = fa
      if value == null then {
        b
      } else {
        f(b, value.asInstanceOf[A])
      }
    }

    def foldRight[A, B](fa: Nullable[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      val value: A | Null = fa
      if value == null then {
        lb
      } else {
        f(value.asInstanceOf[A], lb)
      }
    }

    override def fold[A](fa: Nullable[A])(using A: Monoid[A]): A = {
      val value: A | Null = fa
      if value == null then {
        A.empty
      } else {
        value.asInstanceOf[A]
      }
    }

    override def combineAllOption[A](fa: Nullable[A])(using Semigroup[A]): Option[A] = {
      nonMacroToOption(fa)
    }

    override def toIterable[A](fa: Nullable[A]): Iterable[A] = {
      val value: A | Null = fa
      if value == null then {
        Iterable.empty
      } else {
        Iterable.single(value.asInstanceOf[A])
      }
    }

    override def foldMap[A, B](fa: Nullable[A])(f: A => B)(using B: Monoid[B]): B = {
      val value: A | Null = fa
      if value == null then {
        B.empty
      } else {
        f(value.asInstanceOf[A])
      }
    }

    override def reduceLeftToOption[A, B](fa: Nullable[A])(f: A => B)(g: (B, A) => B): Option[B] = {
      nonMacroToOption(fa).map(f)
    }

    override def reduceRightToOption[A, B](fa: Nullable[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[Option[B]] = {
      Eval.now(nonMacroToOption(fa).map(f))
    }

    override def reduceLeftOption[A](fa: Nullable[A])(f: (A, A) => A): Option[A] = {
      nonMacroToOption(fa)
    }

    override def reduceRightOption[A](fa: Nullable[A])(f: (A, Eval[A]) => Eval[A]): Eval[Option[A]] = {
      Eval.now(nonMacroToOption(fa))
    }

    override def minimumOption[A](fa: Nullable[A])(using Order[A]): Option[A] = {
      nonMacroToOption(fa)
    }

    override def maximumOption[A](fa: Nullable[A])(using Order[A]): Option[A] = {
      nonMacroToOption(fa)
    }

    override def get[A](fa: Nullable[A])(idx: Long): Option[A] = {
      if idx != 0L then {
        None
      } else {
        nonMacroToOption(fa)
      }
    }

    override def contains_[A](fa: Nullable[A], v: A)(using A: Eq[A]): Boolean = {
      val value: A | Null = fa
      if value == null then {
        false
      } else {
        A.eqv(value.asInstanceOf[A], v)
      }
    }

    override def size[A](fa: Nullable[A]): Long = {
      val value: A | Null = fa
      if value == null then {
        0L
      } else {
        1L
      }
    }

    override def find[A](fa: Nullable[A])(f: A => Boolean): Option[A] = {
      val value: A | Null = fa
      if value == null then {
        None
      } else {
        val a = value.asInstanceOf[A]
        if f(a) then {
          Some(a)
        } else {
          None
        }
      }
    }

    override def exists[A](fa: Nullable[A])(p: A => Boolean): Boolean = {
      val value: A | Null = fa
      if value == null then {
        false
      } else {
        p(value.asInstanceOf[A])
      }
    }

    override def forall[A](fa: Nullable[A])(p: A => Boolean): Boolean = {
      val value: A | Null = fa
      if value == null then {
        true
      } else {
        p(value.asInstanceOf[A])
      }
    }

    override def toList[A](fa: Nullable[A]): List[A] = {
      val value: A | Null = fa
      if value == null then {
        Nil
      } else {
        value.asInstanceOf[A] :: Nil
      }
    }

    override def filter_[A](fa: Nullable[A])(p: A => Boolean): List[A] = {
      val value: A | Null = fa
      if value == null then {
        Nil
      } else {
        val a = value.asInstanceOf[A]
        if p(a) then {
          a :: Nil
        } else {
          Nil
        }
      }
    }

    override def takeWhile_[A](fa: Nullable[A])(p: A => Boolean): List[A] = {
      val value: A | Null = fa
      if value == null then {
        Nil
      } else {
        val a = value.asInstanceOf[A]
        if p(a) then {
          a :: Nil
        } else {
          Nil
        }
      }
    }

    override def dropWhile_[A](fa: Nullable[A])(p: A => Boolean): List[A] = {
      val value: A | Null = fa
      if value == null then {
        Nil
      } else {
        val a = value.asInstanceOf[A]
        if p(a) then {
          Nil
        } else {
          a :: Nil
        }
      }
    }

    override def isEmpty[A](fa: Nullable[A]): Boolean = {
      val value: A | Null = fa
      value == null
    }

    override def nonEmpty[A](fa: Nullable[A]): Boolean = {
      !isEmpty(fa)
    }

    override def collectFirst[A, B](fa: Nullable[A])(pf: PartialFunction[A, B]): Option[B] = {
      val value: A | Null = fa
      if value == null then {
        None
      } else {
        val a = value.asInstanceOf[A]
        if pf.isDefinedAt(a) then {
          Some(pf(a))
        } else {
          None
        }
      }
    }

    override def collectFirstSome[A, B](fa: Nullable[A])(f: A => Option[B]): Option[B] = {
      val value: A | Null = fa
      if value == null then {
        None
      } else {
        f(value.asInstanceOf[A])
      }
    }

    override def traverseVoid[G[_], A, B](fa: Nullable[A])(f: A => G[B])(using G: Applicative[G]): G[Unit] = {
      val value: A | Null = fa
      if value == null then {
        G.unit
      } else {
        G.void(f(value.asInstanceOf[A]))
      }
    }

    def traverse[G[_], A, B](fa: Nullable[A])(f: A => G[B])(using G: Applicative[G]): G[Nullable[B]] = {
      val value: A | Null = fa
      if value == null then {
        G.pure(Nullable.empty[B])
      } else {
        val gb: G[B] = f(value.asInstanceOf[A])
        // We can treat `Nullable[B]` as `B | Null`; `widen` is typically a no-op
        // for lawful/correctly implemented Functors.
        val gNullable: G[Nullable[B]] = G.widen[B, B | Null](gb)
        gNullable
      }
    }

    override def traverseTap[G[_], A, B](fa: Nullable[A])(f: A => G[B])(using G: Applicative[G]): G[Nullable[A]] = {
      val value: A | Null = fa
      if value == null then {
        G.pure(Nullable.empty[A])
      } else {
        val a = value.asInstanceOf[A]
        G.as(f(a), a)
      }
    }

    override def sequence[G[_], A](fga: Nullable[G[A]])(using G: Applicative[G]): G[Nullable[A]] = {
      val value: G[A] | Null = fga
      if value == null then {
        G.pure(Nullable.empty[A])
      } else {
        G.widen[A, A | Null](value.asInstanceOf[G[A]])
      }
    }

    override def mapAccumulate[S, A, B](init: S, fa: Nullable[A])(f: (S, A) => (S, B)): (S, Nullable[B]) = {
      val value: A | Null = fa
      if value == null then {
        (init, Nullable.empty[B])
      } else {
        val (next, b) = f(init, value.asInstanceOf[A])
        (next, b)
      }
    }

    override def mapWithIndex[A, B](fa: Nullable[A])(f: (A, Int) => B): Nullable[B] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        f(value.asInstanceOf[A], 0)
      }
    }

    override def traverseWithIndexM[G[_], A, B](
      fa: Nullable[A]
    )(f: (A, Int) => G[B])(using G: Monad[G]): G[Nullable[B]] = {
      val value: A | Null = fa
      if value == null then {
        G.pure(Nullable.empty[B])
      } else {
        G.widen[B, B | Null](f(value.asInstanceOf[A], 0))
      }
    }

    override def zipWithIndex[A](fa: Nullable[A]): Nullable[(A, Int)] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        (value.asInstanceOf[A], 0)
      }
    }

    override def traverseWithLongIndexM[G[_], A, B](
      fa: Nullable[A]
    )(f: (A, Long) => G[B])(using G: Monad[G]): G[Nullable[B]] = {
      val value: A | Null = fa
      if value == null then {
        G.pure(Nullable.empty[B])
      } else {
        G.widen[B, B | Null](f(value.asInstanceOf[A], 0L))
      }
    }

    override def mapWithLongIndex[A, B](fa: Nullable[A])(f: (A, Long) => B): Nullable[B] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        f(value.asInstanceOf[A], 0L)
      }
    }

    override def zipWithLongIndex[A](fa: Nullable[A]): Nullable[(A, Long)] = {
      val value: A | Null = fa
      if value == null then {
        Nullable.empty
      } else {
        (value.asInstanceOf[A], 0L)
      }
    }

    override def updated_[A, B >: A](fa: Nullable[A], idx: Long, b: B): Option[Nullable[B]] = {
      if idx < 0L then {
        None
      } else {
        val value: A | Null = fa
        if value == null then {
          None
        } else if idx == 0L then {
          Some(b: Nullable[B])
        } else {
          None
        }
      }
    }
  }

  /*
   * Keep `fold` as a macro:
   * 1. Strict equality + explicit nulls: we emit a local `CanEqual[T | Null, Null]`,
   *    so the generated null check remains valid under those modes.
   * 2. Performance: `Expr.betaReduce` keeps `fold` codegen close to a plain `if/else`
   *    by reducing the `f(safe)` application at compile time when possible.
   * 3. Compiler stability: the `asInstanceOf[T]` cast in the `else` branch is intentional.
   *    Moving/removing that cast can crash some Scala 3 versions on intersection types:
   *    https://github.com/scala/scala3/issues/25208
   */
  private def foldImpl[T: Type, A: Type](
    nullable: Expr[Nullable[T]],
    ifNull: Expr[A],
    fn: Expr[T => A]
  )(using Quotes): Expr[A] = {
    val nullableUnion: Expr[T | Null] = '{ $nullable.asInstanceOf[T | Null] }

    '{
      val n = $nullableUnion
      given CanEqual[T | Null, Null] = CanEqual.derived
      if n == null then {
        $ifNull
      } else {
        val safe: T = n.asInstanceOf[T]
        ${ Expr.betaReduce('{ $fn(safe) }) }
      }
    }
  }
}

sealed abstract private[data] class NullableInstances extends NullableInstances0 {
  given [A](using A: Order[A]): Order[Nullable[A]] with {
    def compare(na: Nullable[A], nb: Nullable[A]): Int = {
      Nullable.compareNullable(na, nb)
    }

    override def pmin(na: Nullable[A], nb: Nullable[A]): Option[Nullable[A]] = {
      if compare(na, nb) <= 0 then {
        Nullable.toOptionNullable(na)
      } else {
        Nullable.toOptionNullable(nb)
      }
    }

    override def pmax(na: Nullable[A], nb: Nullable[A]): Option[Nullable[A]] = {
      if compare(na, nb) >= 0 then {
        Nullable.toOptionNullable(na)
      } else {
        Nullable.toOptionNullable(nb)
      }
    }
  }
}

private[data] trait NullableInstances0 extends NullableInstances1 {
  given [A](using A: PartialOrder[A]): PartialOrder[Nullable[A]] with {
    def partialCompare(na: Nullable[A], nb: Nullable[A]): Double = {
      Nullable.partialCompareNullable(na, nb)
    }

    override def pmin(na: Nullable[A], nb: Nullable[A]): Option[Nullable[A]] = {
      val c = partialCompare(na, nb)
      // Avoid boxing by calling the primitive static function.
      if java.lang.Double.isNaN(c) then {
        None
      } else if c <= 0.0 then {
        Nullable.toOptionNullable(na)
      } else {
        Nullable.toOptionNullable(nb)
      }
    }

    override def pmax(na: Nullable[A], nb: Nullable[A]): Option[Nullable[A]] = {
      val c = partialCompare(na, nb)
      // Avoid boxing by calling the primitive static function.
      if java.lang.Double.isNaN(c) then {
        None
      } else if c >= 0.0 then {
        Nullable.toOptionNullable(na)
      } else {
        Nullable.toOptionNullable(nb)
      }
    }
  }
}

private[data] trait NullableInstances1 extends NullableInstances2 {
  given [A](using A: Hash[A]): Hash[Nullable[A]] with {
    def eqv(na: Nullable[A], nb: Nullable[A]): Boolean = {
      Nullable.eqvNullable(na, nb)
    }

    def hash(nullable: Nullable[A]): Int = {
      Nullable.hashNullable(nullable)
    }
  }
}

private[data] trait NullableInstances2 {
  given [A](using A: Eq[A]): Eq[Nullable[A]] with {
    def eqv(na: Nullable[A], nb: Nullable[A]): Boolean = {
      Nullable.eqvNullable(na, nb)
    }
  }
}
