package cats
package tests

import cats.data.IorT
import cats.laws.discipline.arbitrary._

class IorTTests extends CatsSuite {

  test("fold with Id consistent with Ior fold") {
    forAll { (iort: IorT[Id, String, Int], fa: String => Long, fb: Int => Long, fab: (String, Int) => Long) =>
      iort.fold(fa, fb, fab) should === (iort.value.fold(fa, fb, fab))
    }
  }

  test("isLeft with Id consistent with Ior isLeft") {
    forAll { (iort: IorT[Id, String, Int]) =>
      iort.isLeft should === (iort.value.isLeft)
    }
  }

  test("isRight with Id consistent with Ior isRight") {
    forAll { (iort: IorT[Id, String, Int]) =>
      iort.isRight should === (iort.value.isRight)
    }
  }

  test("isBoth with Id consistent with Ior isBoth") {
    forAll { (iort: IorT[Id, String, Int]) =>
      iort.isBoth should === (iort.value.isBoth)
    }
  }

  test("isBoth consistent with swap") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.isBoth should === (iort.swap.isBoth)
    }
  }

  test("double swap is noop") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.swap.swap.value should === (iort.value)
    }
  }

  test("getOrElse with Id consistent with Ior getOrElse") {
    forAll { (iort: IorT[Id, String, Int], i: Int) =>
      iort.getOrElse(i) should === (iort.value.getOrElse(i))
    }
  }

  test("getOrElseF with Id consistent with Ior getOrElse") {
    forAll { (iort: IorT[Id, String, Int], i: Int) =>
      iort.getOrElseF(i) should === (iort.value.getOrElse(i))
    }
  }

  test("valueOr with Id consistent with Ior valueOr") {
    forAll { (iort: IorT[Id, String, Int], f: String => Int) =>
      iort.valueOr(f) should === (iort.value.valueOr(f))
    }
  }

  test("forall with Id consistent with Ior forall") {
    forAll { (iort: IorT[Id, String, Int], f: Int => Boolean) =>
      iort.forall(f) should === (iort.value.forall(f))
    }
  }

  test("exists with Id consistent with Ior exists") {
    forAll { (iort: IorT[Id, String, Int], f: Int => Boolean) =>
      iort.exists(f) should === (iort.value.exists(f))
    }
  }

  test("toOption consistent with isLeft") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toOption.isDefined.map(! _) should === (iort.isLeft)
    }
  }

  test("toEither consistent with toOption") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toEither.toOption should === (iort.toOption)
    }
  }

  test("toEither consistent with isLeft") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toEither.isLeft should === (iort.isLeft)
    }
  }

  test("toNested has no loss") {
    forAll { (iort: IorT[List, String, Int]) =>
      iort.toNested.value should === (iort.value)
    }
  }

  test("merge with Id consistent with Ior merge") {
    forAll { (iort: IorT[Id, Int, Int]) =>
      iort.merge should === (iort.value.merge)
    }
  }
}