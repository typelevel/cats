package catsBC

import org.scalatest.suit.AnyFunSuiteLike

class MimaExceptionsTest extends AnyFunSuiteLike {
  test("is binary compatible") {
    MimaExceptions.isBinaryCompatible
  }
}
