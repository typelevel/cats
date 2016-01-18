package cats.tests

import cats.{Applicative, Id, Monad, Monoidal, Show}
import cats.data.{OptionT, Validated, Xor}
import cats.laws.discipline.{ApplicativeTests, FunctorTests, MonadCombineTests, SerializableTests, MonoidalTests, MonadTests}
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.{Arbitrary, Gen}

class OptionTTests extends CatsSuite {

  test("fold and cata consistent") {
    forAll { (o: OptionT[List, Int], s: String, f: Int => String) =>
      o.fold(s)(f) should === (o.cata(s, f))
    }
  }

  test("OptionT[Id, A].fold consistent with Option.fold") {
    forAll { (o: Option[Int], s: String, f: Int => String) =>
      o.fold(s)(f) should === (OptionT[Id, Int](o).fold(s)(f))
    }
  }

  test("OptionT[Id, A].getOrElse consistent with Option.getOrElse") {
    forAll { (o: Option[Int], i: Int) =>
      o.getOrElse(i) should === (OptionT[Id, Int](o).getOrElse(i))
    }
  }

  test("OptionT[Id, A].getOrElseF consistent with Option.getOrElse") {
    forAll { (o: Option[Int], i: Int) =>
      o.getOrElse(i) should === (OptionT[Id, Int](o).getOrElseF(i))
    }
  }

  test("OptionT[Id, A].collect consistent with Option.collect") {
    forAll { (o: Option[Int], f: Int => Option[String]) =>
      val p = Function.unlift(f)
      o.collect(p) should === (OptionT[Id, Int](o).collect(p).value)
    }
  }

  test("OptionT[Id, A].exists consistent with Option.exists") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.exists(f) should === (OptionT[Id, Int](o).exists(f))
    }
  }

  test("OptionT[Id, A].filter consistent with Option.filter") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.filter(f) should === (OptionT[Id, Int](o).filter(f).value)
    }
  }

  test("OptionT[Id, A].filterNot consistent with Option.filterNot") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.filterNot(f) should === (OptionT[Id, Int](o).filterNot(f).value)
    }
  }

  test("OptionT[Id, A].forall consistent with Option.forall") {
    forAll { (o: Option[Int], f: Int => Boolean) =>
      o.forall(f) should === (OptionT[Id, Int](o).forall(f))
    }
  }

  test("OptionT[Id, A].isDefined consistent with Option.isDefined") {
    forAll { o: Option[Int] =>
      o.isDefined should === (OptionT[Id, Int](o).isDefined)
    }
  }

  test("OptionT[Id, A].isEmpty consistent with Option.isEmpty") {
    forAll { o: Option[Int] =>
      o.isEmpty should === (OptionT[Id, Int](o).isEmpty)
    }
  }

  test("orElse and orElseF consistent") {
    forAll { (o1: OptionT[List, Int], o2: OptionT[List, Int]) =>
      o1.orElse(o2) should === (o1.orElseF(o2.value))
    }
  }

  test("flatMap and flatMapF consistent") {
    forAll { (optionT: OptionT[List, Int], f: Int => OptionT[List, Int])  =>
      optionT.flatMap(f) should === (optionT.flatMapF(f(_).value))
    }
  }

  test("OptionT[Id, A].toRight consistent with Xor.fromOption") {
    forAll { (o: OptionT[Id, Int], s: String) =>
      o.toRight(s).value should === (Xor.fromOption(o.value, s))
    }
  }

  test("toRight consistent with isDefined") {
    forAll { (o: OptionT[List, Int], s: String) =>
      o.toRight(s).isRight should === (o.isDefined)
    }
  }

  test("toLeft consistent with isDefined") {
    forAll { (o: OptionT[List, Int], s: String) =>
      o.toLeft(s).isLeft should === (o.isDefined)
    }
  }

  test("isDefined is negation of isEmpty") {
    forAll { (o: OptionT[List, Int]) =>
      o.isDefined should === (o.isEmpty.map(! _))
    }
  }

  test("fromOption") {
    forAll { (o: Option[Int]) =>
      List(o) should === (OptionT.fromOption[List](o).value)
    }
  }

  implicit val iso = MonoidalTests.Isomorphisms.invariant[OptionT[List, ?]]

  checkAll("OptionT[List, Int]", MonoidalTests[OptionT[List, ?]].monoidal[Int, Int, Int])
  checkAll("Monoidal[OptionT[List, ?]]", SerializableTests.serializable(Monoidal[OptionT[List, ?]]))

  test("liftF") {
    forAll { (xs: List[Int]) =>
      xs.map(Option(_)) should ===(OptionT.liftF(xs).value)
    }
  }

  test("show") {
    val xor: String Xor Option[Int] = Xor.right(Some(1))
    OptionT[Xor[String, ?], Int](xor).show should === ("Xor.Right(Some(1))")
  }

  test("implicit Show[OptionT] instance and explicit show method are consistent") {
    forAll { optionT: OptionT[List, Int] =>
      optionT.show should === (implicitly[Show[OptionT[List, Int]]].show(optionT))
    }
  }

  test("transform consistent with value.map") {
    forAll { (o: OptionT[List, Int], f: Option[Int] => Option[String]) =>
      o.transform(f) should === (OptionT(o.value.map(f)))
    }
  }

  test("subflatMap consistent with value.map+flatMap") {
    forAll { (o: OptionT[List, Int], f: Int => Option[String]) =>
      o.subflatMap(f) should === (OptionT(o.value.map(_.flatMap(f))))
    }
  }

  checkAll("Monad[OptionT[List, Int]]", MonadTests[OptionT[List, ?]].monad[Int, Int, Int])
  checkAll("Monad[OptionT[List, ?]]", SerializableTests.serializable(Monad[OptionT[List, ?]]))

  {
    implicit val F = ListWrapper.functor
    checkAll("Functor[OptionT[ListWrapper, ?]]", FunctorTests[OptionT[ListWrapper, ?]].functor[Int, Int, Int])
  }
}
