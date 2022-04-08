/*
 * Copyright (c) 2022 Typelevel
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

package algebra

import scala.annotation.nowarn

/**
 * Priority is a type class for prioritized implicit search.
 *
 * This type class will attempt to provide an implicit instance of `P`
 * (the preferred type). If that type is not available it will
 * fallback to `F` (the fallback type). If neither type is available
 * then a `Priority[P, F]` instance will not be available.
 *
 * This type can be useful for problems where multiple algorithms can
 * be used, depending on the type classes available.
 */
sealed trait Priority[+P, +F] {

  import Priority.{Fallback, Preferred}

  def fold[B](f1: P => B)(f2: F => B): B =
    this match {
      case Preferred(x) => f1(x)
      case Fallback(y)  => f2(y)
    }

  def join[U >: P with F]: U =
    fold(_.asInstanceOf[U])(_.asInstanceOf[U])

  def bimap[P2, F2](f1: P => P2)(f2: F => F2): Priority[P2, F2] =
    this match {
      case Preferred(x) => Preferred(f1(x))
      case Fallback(y)  => Fallback(f2(y))
    }

  def toEither: Either[P, F] =
    fold[Either[P, F]](p => Left(p))(f => Right(f))

  def isPreferred: Boolean =
    fold(_ => true)(_ => false)

  def isFallback: Boolean =
    fold(_ => false)(_ => true)

  def getPreferred: Option[P] =
    fold[Option[P]](p => Some(p))(_ => None)

  def getFallback: Option[F] =
    fold[Option[F]](_ => None)(f => Some(f))
}

object Priority extends FindPreferred {

  case class Preferred[P](get: P) extends Priority[P, Nothing]
  case class Fallback[F](get: F) extends Priority[Nothing, F]

  def apply[P, F](implicit ev: Priority[P, F]): Priority[P, F] = ev
}

private[algebra] trait FindPreferred extends FindFallback {
  @nowarn("msg=deprecated")
  implicit def preferred[P](implicit ev: P): Priority[P, Nothing] =
    Priority.Preferred(ev)
}

private[algebra] trait FindFallback {
  @nowarn("msg=deprecated")
  implicit def fallback[F](implicit ev: F): Priority[Nothing, F] =
    Priority.Fallback(ev)
}
