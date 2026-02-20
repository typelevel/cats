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

import cats.{Eq, Eval, Foldable, Hash, Monoid, Order, PartialOrder, Semigroup, Show}
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

    /**
     * Transforms the non-null value.
     *
     * This is not a lawful `Functor` map. Because `Nullable[Nullable[A]]` collapses to
     * `Nullable[A]`, composition can break:
     *
     * {{{
     * val n: Nullable[Nullable[Int]] = Nullable(Nullable(1))
     * val f: Int => String = _ => null
     * val g: String => Int = _ => 42
     *
     * val lhs = n.transform(_.transform(f)).transform(_.transform(g))
     * val rhs = n.transform(_.transform(f.andThen(g)))
     *
     * // lhs is null, rhs is 42
     * }}}
     */
    inline def transform[B](inline f: A => B): Nullable[B] = {
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

  extension [A](inline nested: Nullable[Nullable[A]]) {
    inline def flatten: Nullable[A] = {
      nested
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
  private[data] inline def nonMacroToOption[A](n: Nullable[A]): Option[A] = {
    val value: A | Null = n
    if value == null then {
      None
    } else {
      Some(value.asInstanceOf[A])
    }
  }

  private[data] inline def nonMacroToNullableOption[A](n: Nullable[A]): Option[Nullable[A]] = {
    val value: A | Null = n
    if value == null then {
      None
    } else {
      Some(n)
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
        java.lang.String.valueOf(null: AnyRef)
      } else {
        A.show(value.asInstanceOf[A])
      }
    }
  }

  /*
   * These methods intentionally use direct null checks instead of `fa.fold(...)`.
   * `fold` is implemented as an inline macro in this same source file, and Scala 3
   * rejects calls from non-inline methods to a macro defined in the same file.
   */
  given catsDataFoldableForNullable: Foldable[Nullable] with {
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
        Nullable.nonMacroToNullableOption(na)
      } else {
        Nullable.nonMacroToNullableOption(nb)
      }
    }

    override def pmax(na: Nullable[A], nb: Nullable[A]): Option[Nullable[A]] = {
      if compare(na, nb) >= 0 then {
        Nullable.nonMacroToNullableOption(na)
      } else {
        Nullable.nonMacroToNullableOption(nb)
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
        Nullable.nonMacroToNullableOption(na)
      } else {
        Nullable.nonMacroToNullableOption(nb)
      }
    }

    override def pmax(na: Nullable[A], nb: Nullable[A]): Option[Nullable[A]] = {
      val c = partialCompare(na, nb)
      // Avoid boxing by calling the primitive static function.
      if java.lang.Double.isNaN(c) then {
        None
      } else if c >= 0.0 then {
        Nullable.nonMacroToNullableOption(na)
      } else {
        Nullable.nonMacroToNullableOption(nb)
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
