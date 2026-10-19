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
package tests

import cats.data.Ior
import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary._
import cats.syntax.reducible._
import org.scalacheck.Prop

class BireducibleSuite extends CatsSuite {
  import BireducibleSuite._

  // Bireducible.bireduceLeft tests
  //
  property("Bireducible.bireduceLeft[IorNel]") {
    Prop.forAll { (testee: NonEmptyList[String Ior List[Byte]]) =>
      val expected = testee.reduce
      val obtained = Bireducible.bireduceLeft(IorNel(testee))(_ + _, _ ::: _)

      assertEquals(obtained, expected)
    }
  }
  property("Bireducible.bireduceLeft[NelIor]") {
    Prop.forAll { (testee: NonEmptyList[List[Char]] Ior NonEmptyList[String]) =>
      val expected = testee.bimap(_.reduce, _.reduce)
      val obtained = Bireducible.bireduceLeft(NelIor(testee))(_ ::: _, _ + _)

      assertEquals(obtained, expected)
    }
  }
  // Bireducible.bireduceRight tests
  //
  property("Bireducible.bireduceRight[IorNel]") {
    Prop.forAll { (testee: NonEmptyList[String Ior List[Char]]) =>
      val expected = testee.reduce
      val obtained =
        Bireducible
          .bireduceRight(IorNel(testee))(
            (a, ea) => ea.map(a + _),
            (b, eb) => eb.map(b ::: _)
          )
          .value

      assertEquals(obtained, expected)
    }
  }
  property("Bireducible.bireduceRight[NelIor]") {
    Prop.forAll { (testee: NonEmptyList[List[Byte]] Ior NonEmptyList[String]) =>
      val expected = testee.bimap(_.reduce, _.reduce)
      val obtained =
        Bireducible
          .bireduceRight(NelIor(testee))(
            (a, ea) => ea.map(a ::: _),
            (b, eb) => eb.map(b + _)
          )
          .value

      assertEquals(obtained, expected)
    }
  }
  // Bireducible.bireduceMap tests
  //
  property("Bireducible.bireduceMap[IorNel]") {
    Prop.forAll { (testee: NonEmptyList[String Ior String]) =>
      val expected = testee.map(_.merge).reduce
      val obtained = Bireducible.bireduceMap(IorNel(testee))(identity, identity)

      assertNoDiff(obtained, expected)
    }
  }
  property("Bireducible.bireduceMap[NelIor]") {
    Prop.forAll { (testee: NonEmptyList[String] Ior NonEmptyList[String]) =>
      val expected = testee.bimap(_.reduce, _.reduce).merge
      val obtained = Bireducible.bireduceMap(NelIor(testee))(identity, identity)

      assertNoDiff(obtained, expected)
    }
  }
  // Bireducible.bireduce tests
  //
  property("Bireducible.bireduce[IorNel]") {
    Prop.forAll { (testee: NonEmptyList[List[Byte] Ior List[Char]]) =>
      val expected = testee.reduce
      val obtained = Bireducible.bireduce(IorNel(testee))

      assertEquals(obtained, expected)
    }
  }
  property("Bireducible.bireduce[NelIor]") {
    Prop.forAll { (testee: NonEmptyList[List[Char]] Ior NonEmptyList[List[Byte]]) =>
      val expected = testee.bimap(_.reduce, _.reduce)
      val obtained = Bireducible.bireduce(NelIor(testee))

      assertEquals(obtained, expected)
    }
  }
}

object BireducibleSuite {

  /** `Bireducible` that can be collapsed row by row.
    */
  final case class IorNel[A, B](value: NonEmptyList[A Ior B]) extends AnyVal

  /** `Bireducible` that can be collapsed by collapsing one of the columns followed by the other one.
    */
  final case class NelIor[A, B](value: NonEmptyList[A] Ior NonEmptyList[B]) extends AnyVal

  implicit val testIorNelBireducible: Bireducible[IorNel] = new Bireducible[IorNel] {

    override def bifoldLeft[A, B, C](fab: IorNel[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C = ???

    override def bifoldRight[A, B, C](fab: IorNel[A, B], c: Eval[C])(
      f: (A, Eval[C]) => Eval[C],
      g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = ???

    override def bireduceLeftTo[A, B, C](fab: IorNel[A, B])(
      ma: A => C,
      mb: B => C
    )(
      mca: (C, A) => C,
      mcb: (C, B) => C
    ): C = {
      fab.value.reduceLeftTo { ab =>
        ab.fold(ma, mb, (a, b) => mcb(ma(a), b))
      } { (c, ab) =>
        ab.fold(mca(c, _), mcb(c, _), (a, b) => mcb(mca(c, a), b))
      }
    }

    override def bireduceRightTo[A, B, C](
      fab: IorNel[A, B]
    )(
      ma: A => Eval[C],
      mb: B => Eval[C]
    )(
      mac: (A, Eval[C]) => Eval[C],
      mbc: (B, Eval[C]) => Eval[C]
    ): Eval[C] =
      fab.value.reduceRightTo { ab =>
        // Enforcing `.value` here because this parameter is defined as `A => B`.
        // TODO: consider making `reduceRightTo` to take `A => Eval[B]` to improve its composability.
        ab.fold(ma, mb, (a, b) => mbc(b, ma(a))).value
      } { (ab, ec) =>
        ab.fold(a => mac(a, ec), b => mbc(b, ec), (a, b) => mbc(b, mac(a, ec)))
      }
  }

  implicit val testNelIorBireducible: Bireducible[NelIor] = new Bireducible[NelIor] {

    override def bifoldLeft[A, B, C](fab: NelIor[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C = ???

    override def bifoldRight[A, B, C](fab: NelIor[A, B], c: Eval[C])(
      f: (A, Eval[C]) => Eval[C],
      g: (B, Eval[C]) => Eval[C]
    ): Eval[C] = ???

    override def bireduceLeftTo[A, B, C](fab: NelIor[A, B])(
      ma: A => C,
      mb: B => C
    )(
      mca: (C, A) => C,
      mcb: (C, B) => C
    ): C =
      fab.value.fold(
        _.reduceLeftTo(ma)(mca),
        _.reduceLeftTo(mb)(mcb),
        { (as, bs) =>
          val ca = as.reduceLeftTo(ma)(mca)
          val cb = bs.foldLeft(ca)(mcb)
          cb
        }
      )

    override def bireduceRightTo[A, B, C](fab: NelIor[A, B])(
      ma: A => Eval[C],
      mb: B => Eval[C]
    )(
      mac: (A, Eval[C]) => Eval[C],
      mbc: (B, Eval[C]) => Eval[C]
    ): Eval[C] =
      fab.value.fold(
        _.reduceRightTo(ma(_).value)(mac),
        _.reduceRightTo(mb(_).value)(mbc),
        { (as, bs) =>
          val ca = as.reduceRightTo(ma(_).value)(mac)
          val cb = bs.foldRight(ca)(mbc)
          cb
        }
      )
  }
}
