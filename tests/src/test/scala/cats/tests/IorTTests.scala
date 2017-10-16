package cats
package tests

import cats.data.{Ior, IorT}
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

  test("IorT.left with Option isLeft") {
    forAll { (option: Option[String]) =>
      IorT.left[Int](option).isLeft should === (option.map(_ => true))
    }
  }

  test("IorT.leftT isLeft") {
    forAll { (s: String) =>
      IorT.leftT[Option, Int](s).isLeft should === (Some(true))
    }
  }

  test("IorT.right with Option isRight") {
    forAll { (option: Option[Int]) =>
      IorT.right[String](option).isRight should === (option.map(_ => true))
    }
  }

  test("IorT.rightT consistent with IorT.pure") {
    forAll { (i: Int) =>
      IorT.rightT[Option, String](i).value should === (IorT.pure[Option, String](i).value)
    }
  }

  test("IorT.both isBoth with Option consistent with Option zip") {
    forAll { (optionS: Option[String], optionI: Option[Int]) =>
      IorT.both(optionS, optionI).isBoth should === (optionS.zip(optionI).headOption.map(_ => true))
    }
  }

  test("IorT.bothT isBoth") {
    forAll { (s: String, i: Int) =>
      IorT.bothT[Option](s, i).isBoth should === (Some(true))
    }
  }

  test("IorT.pure isRight") {
    forAll { (i: Int) =>
      IorT.rightT[Option, String](i).isRight should === (Some(true))
    }
  }

  test("IorT.liftF consistent with IorT.right") {
    forAll { (option: Option[Int]) =>
      IorT.liftF[Option, String, Int](option).value should === (IorT.right[String](option).value)
    }
  }

  test("IorT.fromIor with Id is noop") {
    forAll { (ior: Ior[String, Int]) =>
      IorT.fromIor[Id](ior).value should === (ior)
    }
  }

  test("IorT.fromEither toEither is noop") {
    forAll { (either: Either[String, Int]) =>
      IorT.fromEither[Id](either).value.toEither should === (either)
    }
  }

  test("IorT.fromEitherF toEither is noop") {
    forAll { (either: Either[String, Int]) =>
      IorT.fromEitherF[Id, String, Int](either).value.toEither should === (either)
    }
  }

  test("IorT.fromOption isLeft consistent with Option isEmpty") {
    forAll { (option: Option[Int], s: String) =>
      IorT.fromOption[Id](option, s).isLeft should === (option.isEmpty)
    }
  }

  test("IorT.fromOptionF isLeft consistent with Option isEmpty") {
    forAll { (option: Option[Int], s: String) =>
      IorT.fromOptionF[Id, String, Int](option, s).isLeft should === (option.isEmpty)
    }
  }

  test("IorT.cond isRight equals test") {
    forAll { (test: Boolean, s: String, i: Int) =>
      val iort = IorT.cond[Id](test, s, i)
      iort.isRight && !iort.isLeft && !iort.isBoth should === (test)
    }
  }
}