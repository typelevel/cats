package cats.tests

import cats.{~>, Id}
import cats.arrow.FunctionK
import cats.arrow.FunctionK.*

class FunctionKPlatformSuite extends CatsSuite {

  test("Conversion from FunctionK to polymorphic function") {
    val id: Option ~> Option = FunctionK.id[Option]
    def foo(f: [A] => Option[A] => Option[A]): Option[String] = f(Some("foo"))

    assert(foo(id) == Some("foo"))
  }
}
