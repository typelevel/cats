package catsBC

import org.scalatest.FunSuite

class MimaExceptionsTest extends FunSuite {
  test("is binary compatible") {
    MimaExceptions.isBinaryCompatible
  }
}
