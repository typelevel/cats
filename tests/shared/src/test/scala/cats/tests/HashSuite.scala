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

package cats.tests

import cats.{Contravariant, Invariant}
import cats.kernel.Hash
import cats.laws.discipline.{DeferTests, MiniInt}
import cats.laws.discipline.arbitrary.*
import cats.laws.discipline.eq.*
import cats.syntax.hash.*

class HashSuite extends CatsSuite {

  {
    Invariant[Hash]
    Contravariant[Hash]
  }

  test("hash extension method is consistent with hashCode for Int") {
    assert(1.hash == 1.hashCode)
  }

  test("hash extension method is consistent with hashCode for String") {
    assert("ABC".hash == "ABC".hashCode)
  }

  test("fromUniversalHashCode should be consistent with hashCode on non-null references") {
    class ExplicitHashCode() { override val hashCode = 42 }
    val hash = Hash.fromUniversalHashCode[ExplicitHashCode]
    assertEquals(hash.hash(new ExplicitHashCode()), 42)
  }

  test("fromUniversalHashCode should be null safe") {
    val hash = Hash.fromUniversalHashCode[AnyRef]
    assertEquals(hash.hash(null), 0)
  }

  checkAll("Defer[Hash]", DeferTests[Hash].defer[MiniInt])
}
