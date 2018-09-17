package cats
package BC
import org.scalatest.{FunSuite}


class MimaExceptionsTest extends FunSuite {
  test("is binary compatible") {
    assert(MimaExceptions.isBinaryCompatible)
  }
}
