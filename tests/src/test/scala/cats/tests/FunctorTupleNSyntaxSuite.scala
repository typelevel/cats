package cats.tests

import cats.syntax.functor._
import cats.laws.discipline.arbitrary._
import org.scalacheck.Prop._

class FunctorTupleNSyntaxSuite extends CatsSuite {
  test("_1F, _2F, _3F works for Tuple3") {
    forAll { (l: List[(Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
    }
  }

  test("_1F, _2F, _3F, _4F works for Tuple4") {
    forAll { (l: List[(Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F works for Tuple5") {
    forAll { (l: List[(Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F works for Tuple6") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F works for Tuple6") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F works for Tuple7") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, 8F works for Tuple8") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F works for Tuple9") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F works for Tuple10") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F works for Tuple11") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F works for Tuple12") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F works for Tuple13") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F works for Tuple14") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F, _15F works for Tuple15") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
      assertEquals(l._15F, l.map(_._15))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F, _15F, _16F works for Tuple16") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
      assertEquals(l._15F, l.map(_._15))
      assertEquals(l._16F, l.map(_._16))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F, _15F, _16F, _17F works for Tuple17") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
      assertEquals(l._15F, l.map(_._15))
      assertEquals(l._16F, l.map(_._16))
      assertEquals(l._17F, l.map(_._17))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F, _15F, _16F, _17F, _18F works for Tuple18") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
      assertEquals(l._15F, l.map(_._15))
      assertEquals(l._16F, l.map(_._16))
      assertEquals(l._17F, l.map(_._17))
      assertEquals(l._18F, l.map(_._18))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F, _15F, _16F, _17F, _18F, _19F works for Tuple19") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
      assertEquals(l._15F, l.map(_._15))
      assertEquals(l._16F, l.map(_._16))
      assertEquals(l._17F, l.map(_._17))
      assertEquals(l._18F, l.map(_._18))
      assertEquals(l._19F, l.map(_._19))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F, _15F, _16F, _17F, _18F, _19F, _20F works for Tuple20") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
      assertEquals(l._15F, l.map(_._15))
      assertEquals(l._16F, l.map(_._16))
      assertEquals(l._17F, l.map(_._17))
      assertEquals(l._18F, l.map(_._18))
      assertEquals(l._19F, l.map(_._19))
      assertEquals(l._20F, l.map(_._20))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F, _15F, _16F, _17F, _18F, _19F, _20F, _21F works for Tuple21") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
      assertEquals(l._15F, l.map(_._15))
      assertEquals(l._16F, l.map(_._16))
      assertEquals(l._17F, l.map(_._17))
      assertEquals(l._18F, l.map(_._18))
      assertEquals(l._19F, l.map(_._19))
      assertEquals(l._20F, l.map(_._20))
      assertEquals(l._21F, l.map(_._21))
    }
  }

  test("_1F, _2F, _3F, _4F, _5F, _6F, _7F, _8F, _9F, _10F, _11F, _12F, _13F, _14F, _15F, _16F, _17F, _18F, _19F, _20F, _21F, _22F works for Tuple22") {
    forAll { (l: List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)]) =>
      assertEquals(l._1F, l.map(_._1))
      assertEquals(l._2F, l.map(_._2))
      assertEquals(l._3F, l.map(_._3))
      assertEquals(l._4F, l.map(_._4))
      assertEquals(l._5F, l.map(_._5))
      assertEquals(l._6F, l.map(_._6))
      assertEquals(l._7F, l.map(_._7))
      assertEquals(l._8F, l.map(_._8))
      assertEquals(l._9F, l.map(_._9))
      assertEquals(l._10F, l.map(_._10))
      assertEquals(l._11F, l.map(_._11))
      assertEquals(l._12F, l.map(_._12))
      assertEquals(l._13F, l.map(_._13))
      assertEquals(l._14F, l.map(_._14))
      assertEquals(l._15F, l.map(_._15))
      assertEquals(l._16F, l.map(_._16))
      assertEquals(l._17F, l.map(_._17))
      assertEquals(l._18F, l.map(_._18))
      assertEquals(l._19F, l.map(_._19))
      assertEquals(l._20F, l.map(_._20))
      assertEquals(l._21F, l.map(_._21))
      assertEquals(l._22F, l.map(_._22))
    }
  }
}
