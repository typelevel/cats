package cats
package tests

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}


class StringTests extends CatsSuite {

  implicit val eq0 = new Eq[NumberFormatException] {
    override def eqv(x: NumberFormatException, y: NumberFormatException): Boolean = x.getMessage == y.getMessage
  }

  implicit val eq1 = new Eq[IllegalArgumentException] {
    override def eqv(x: IllegalArgumentException, y: IllegalArgumentException): Boolean = x.getMessage == y.getMessage
  }

  test("show") {
    "hello world".show should ===("hello world")
    "".show should ===("")
    forAll { s: String =>
      s.show should ===(s)
    }
  }

  test("parseInt") {
    "123".parseInt should ===(123.right[NumberFormatException])

    "blah".parseInt should ===(new NumberFormatException("For input string: \"blah\"").left)

    forAll { i: Int =>
      i.toString.parseInt should ===(i.right[NumberFormatException])
    }

    forAll { i: Int =>
      i.toString.parseInt.toOption should ===(i.toString.parseIntOption)
    }

  }

  test("parseLong") {
    "123".parseLong should ===(123L.right[NumberFormatException])

    "blah".parseLong should ===(new NumberFormatException("For input string: \"blah\"").left)

    forAll { i: Long =>
      i.toString.parseLong should ===(i.right[NumberFormatException])
    }

    forAll { i: Long =>
      i.toString.parseLong.toOption should ===(i.toString.parseLongOption)
    }

  }

  test("parseShort") {
    "123".parseShort should ===(123.toShort.right[NumberFormatException])

    "blah".parseShort should ===(new NumberFormatException("For input string: \"blah\"").left)

    forAll { i: Short =>
      i.toString.parseShort should ===(i.right[NumberFormatException])
    }

    forAll { i: Short =>
      i.toString.parseShort.toOption should ===(i.toString.parseShortOption)
    }

  }

  test("parseDouble") {
    "123.1".parseDouble should ===(123.1.right[NumberFormatException])

    "blah".parseDouble should ===(new NumberFormatException("For input string: \"blah\"").left)

    forAll { i: Double =>
      i.toString.parseDouble should ===(i.right[NumberFormatException])
    }

    forAll { i: Double =>
      i.toString.parseDouble.toOption should ===(i.toString.parseDoubleOption)
    }
  }

  test("parseFloat") {
    "123.1".parseFloat should ===(123.1f.right[NumberFormatException])

    "blah".parseFloat should ===(new NumberFormatException("For input string: \"blah\"").left)

    forAll { i: Float =>
      i.toString.parseFloat should ===(i.right[NumberFormatException])
    }

    forAll { i: Float =>
      i.toString.parseFloat.toOption should ===(i.toString.parseFloatOption)
    }
  }

  test("parseByte") {
    "123".parseByte should ===(123.toByte.right[NumberFormatException])

    "blah".parseByte should ===(new NumberFormatException("For input string: \"blah\"").left)

    forAll { i: Byte =>
      i.toString.parseByte should ===(i.right[NumberFormatException])
    }

    forAll { i: Byte =>
      i.toString.parseByte.toOption should ===(i.toString.parseByteOption)
    }
  }

  test("parseBoolean") {
    "true".parseBoolean should ===(true.right[IllegalArgumentException])
    "true".parseBoolean.toOption should ===("true".parseBooleanOption)
    "false".parseBoolean should ===(false.right[IllegalArgumentException])
    "false".parseBoolean.toOption should ===("false".parseBooleanOption)

    "TRUE".parseBoolean should ===("true".parseBoolean)
    "FALSE".parseBoolean should ===("false".parseBoolean)

    val stringGen: Gen[String] = Arbitrary.arbString.arbitrary.filter(s => !s.equalsIgnoreCase("true") && !s.equalsIgnoreCase("false"))

    forAll(stringGen) { s: String =>
      s.parseBoolean should ===(new IllegalArgumentException("For input string: \"" + s + "\"").left)
    }
  }

}
