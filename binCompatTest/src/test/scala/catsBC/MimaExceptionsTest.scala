package catsBC

import org.scalatest.funsuite.AnyFunSuiteLike
import cats.data.CatsDataPackagePrivateMimaExceptions

class MimaExceptionsTest extends AnyFunSuiteLike {
  test("is binary compatible") {
    MimaExceptions.isBinaryCompatible
    CatsDataPackagePrivateMimaExceptions.isBinaryCompatible
  }
}
