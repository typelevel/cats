package cats.tests

import cats.{Id, Monad}
import cats.data.{OptionT, Xor}
import cats.laws.discipline.{MonadTests, SerializableTests}
import cats.laws.discipline.arbitrary._
import org.scalacheck.{Arbitrary, Gen, Prop}, Prop.forAll

class OptionTTests extends CatsSuite {

  test("fold and cata consistent")(check {
    forAll { (o: OptionT[List, Int], s: String, f: Int => String) =>
      o.fold(s)(f) == o.cata(s, f)
    }
  })

  test("OptionT[Id, A].fold consistent with Option.fold")(check {
    forAll { (o: Option[Int], s: String, f: Int => String) =>
      o.fold(s)(f) == OptionT[Id, Int](o).fold(s)(f)
    }
  })

  test("OptionT[Id, A].getOrElse consistent with Option.getOrElse")(check {
    forAll { (o: Option[Int], i: Int) =>
      o.getOrElse(i) == OptionT[Id, Int](o).getOrElse(i)
    }
  })

  test("OptionT[Id, A].getOrElseF consistent with Option.getOrElse")(check {
    forAll { (o: Option[Int], i: Int) =>
      o.getOrElse(i) == OptionT[Id, Int](o).getOrElseF(i)
    }
  })

  test("OptionT[Id, A].collect consistent with Option.collect")(check {
    forAll { (o: Option[Int], f: Int => Option[String]) =>
      val p = Function.unlift(f)
      o.collect(p) == OptionT[Id, Int](o).collect(p).value
    }
  })

  test("OptionT[Id, A].exists consistent with Option.exists")(check {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.exists(f) == OptionT[Id, Int](o).exists(f)
    }
  })

  test("OptionT[Id, A].filter consistent with Option.filter")(check {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.filter(f) == OptionT[Id, Int](o).filter(f).value
    }
  })

  test("OptionT[Id, A].filterNot consistent with Option.filterNot")(check {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.filterNot(f) == OptionT[Id, Int](o).filterNot(f).value
    }
  })

  test("OptionT[Id, A].forall consistent with Option.forall")(check {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.forall(f) == OptionT[Id, Int](o).forall(f)
    }
  })

  test("OptionT[Id, A].isDefined consistent with Option.isDefined")(check {
    forAll { o: Option[Int] =>
      o.isDefined == OptionT[Id, Int](o).isDefined
    }
  })

  test("OptionT[Id, A].isEmpty consistent with Option.isEmpty")(check {
    forAll { o: Option[Int] =>
      o.isEmpty == OptionT[Id, Int](o).isEmpty
    }
  })

  test("orElse and orElseF consistent")(check {
    forAll { (o1: OptionT[List, Int], o2: OptionT[List, Int]) =>
      o1.orElse(o2) == o1.orElseF(o2.value)
    }
  })

  test("OptionT[Id, A].toRight consistent with Xor.fromOption")(check {
    forAll { (o: OptionT[Id, Int], s: String) =>
      o.toRight(s).value == Xor.fromOption(o.value, s)
    }
  })

  test("toRight consistent with isDefined")(check {
    forAll { (o: OptionT[List, Int], s: String) =>
      o.toRight(s).isRight == o.isDefined
    }
  })

  test("toLeft consistent with isDefined")(check {
    forAll { (o: OptionT[List, Int], s: String) =>
      o.toLeft(s).isLeft == o.isDefined
    }
  })

  test("isDefined is negation of isEmpty")(check {
    forAll { (o: OptionT[List, Int]) =>
      o.isDefined == o.isEmpty.map(! _)
    }
  })

  checkAll("OptionT[List, Int]", MonadTests[OptionT[List, ?]].monad[Int, Int, Int])
  checkAll("MonadOptionT[List, ?]]", SerializableTests.serializable(Monad[OptionT[List, ?]]))
}
