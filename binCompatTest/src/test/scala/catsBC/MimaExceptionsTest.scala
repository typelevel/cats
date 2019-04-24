package catsBC

import org.scalatest.funsuite.AnyFunSuiteLike

class MimaExceptionsTest extends AnyFunSuiteLike {
  test("is binary compatible") {
    MimaExceptions.isBinaryCompatible
  }
}
