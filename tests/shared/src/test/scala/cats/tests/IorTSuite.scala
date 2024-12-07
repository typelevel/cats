/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats.tests

import cats.*
import cats.data.{Ior, IorT}
import cats.kernel.{Eq, Monoid, Order, Semigroup}
import cats.kernel.laws.discipline.{EqTests, MonoidTests, SemigroupTests}
import cats.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.eq.*
import org.scalacheck.Prop.*
import cats.kernel.laws.discipline.OrderTests

class IorTSuite extends CatsSuite {

  checkAll("IorT[Eval, String, *]", DeferTests[IorT[Eval, String, *]].defer[Int])

  {
    implicit val F: Functor[ListWrapper] = ListWrapper.functor

    checkAll("IorT[ListWrapper, *, *]",
             BifunctorTests[IorT[ListWrapper, *, *]].bifunctor[Int, Int, Int, String, String, String]
    )
    checkAll("Bifunctor[IorT[ListWrapper, *, *]]", SerializableTests.serializable(Bifunctor[IorT[ListWrapper, *, *]]))

    checkAll("IorT[ListWrapper, Int, *]", FunctorTests[IorT[ListWrapper, Int, *]].functor[Int, Int, Int])
    checkAll("Functor[IorT[ListWrapper, Int, *]]", SerializableTests.serializable(Functor[IorT[ListWrapper, Int, *]]))
  }

  {
    implicit val F: Traverse[ListWrapper] = ListWrapper.traverse

    checkAll("IorT[ListWrapper, Int, *]",
             TraverseTests[IorT[ListWrapper, Int, *]].traverse[Int, Int, Int, Int, Option, Option]
    )
    checkAll("Traverse[IorT[ListWrapper, Int, *]]", SerializableTests.serializable(Traverse[IorT[ListWrapper, Int, *]]))
  }

  {
    implicit val F: Monad[ListWrapper] = ListWrapper.monad

    checkAll("IorT[ListWrapper, String, Int]",
             MonadErrorTests[IorT[ListWrapper, String, *], String].monadError[Int, Int, Int]
    )
    checkAll("MonadError[IorT[List, *, *]]",
             SerializableTests.serializable(MonadError[IorT[ListWrapper, String, *], String])
    )
  }

  {
    checkAll("IorT[Option, String, String]",
             MonadErrorTests[IorT[Option, String, *], Unit].monadError[String, String, String]
    )
    checkAll("MonadError[IorT[Option, *, *]]",
             SerializableTests.serializable(MonadError[IorT[Option, String, *], Unit])
    )
  }

  {
    implicit val F: Foldable[ListWrapper] = ListWrapper.foldable

    checkAll("IorT[ListWrapper, Int, *]", FoldableTests[IorT[ListWrapper, Int, *]].foldable[Int, Int])
    checkAll("Foldable[IorT[ListWrapper, Int, *]]", SerializableTests.serializable(Foldable[IorT[ListWrapper, Int, *]]))
  }

  {
    implicit val F: Order[ListWrapper[Ior[String, Int]]] = ListWrapper.order[Ior[String, Int]]

    checkAll("IorT[ListWrapper, String, Int]", OrderTests[IorT[ListWrapper, String, Int]].order)
    checkAll("Order[IorT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Order[IorT[ListWrapper, String, Int]])
    )
  }

  {
    implicit val F: Semigroup[ListWrapper[Ior[String, Int]]] = ListWrapper.semigroup[Ior[String, Int]]

    checkAll("IorT[ListWrapper, String, Int]", SemigroupTests[IorT[ListWrapper, String, Int]].semigroup)
    checkAll("Semigroup[IorT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Semigroup[IorT[ListWrapper, String, Int]])
    )
  }

  {
    implicit val F: Monoid[ListWrapper[Ior[String, Int]]] = ListWrapper.monoid[Ior[String, Int]]

    checkAll("IorT[ListWrapper, String, Int]", MonoidTests[IorT[ListWrapper, String, Int]].monoid)
    checkAll("Monoid[IorT[ListWrapper, String, Int]]",
             SerializableTests.serializable(Monoid[IorT[ListWrapper, String, Int]])
    )
  }

  {
    implicit val F: Eq[ListWrapper[Ior[String, Int]]] = ListWrapper.eqv[Ior[String, Int]]

    checkAll("IorT[ListWrapper, String, Int]", EqTests[IorT[ListWrapper, String, Int]].eqv)
    checkAll("Eq[IorT[ListWrapper, String, Int]]", SerializableTests.serializable(Eq[IorT[ListWrapper, String, Int]]))
  }

  test("fold with Id consistent with Ior fold") {
    forAll { (iort: IorT[Id, String, Int], fa: String => Long, fb: Int => Long, fab: (String, Int) => Long) =>
      assert(iort.fold(fa, fb, fab) === (iort.value.fold(fa, fb, fab)))
    }
  }

  test("foldF with Id consistent with Ior fold") {
    forAll { (iort: IorT[Id, String, Int], fa: String => Long, fb: Int => Long, fab: (String, Int) => Long) =>
      assert(iort.foldF(fa, fb, fab) === (iort.value.fold(fa, fb, fab)))
    }
  }

  test("isLeft with Id consistent with Ior isLeft") {
    forAll { (iort: IorT[Id, String, Int]) =>
      assert(iort.isLeft === (iort.value.isLeft))
    }
  }

  test("isRight with Id consistent with Ior isRight") {
    forAll { (iort: IorT[Id, String, Int]) =>
      assert(iort.isRight === (iort.value.isRight))
    }
  }

  test("isBoth with Id consistent with Ior isBoth") {
    forAll { (iort: IorT[Id, String, Int]) =>
      assert(iort.isBoth === (iort.value.isBoth))
    }
  }

  test("isBoth consistent with swap") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.isBoth === (iort.swap.isBoth))
    }
  }

  test("double swap is noop") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.swap.swap.value === (iort.value))
    }
  }

  test("getOrElse with Id consistent with Ior getOrElse") {
    forAll { (iort: IorT[Id, String, Int], i: Int) =>
      assert(iort.getOrElse(i) === (iort.value.getOrElse(i)))
    }
  }

  test("getOrElseF with Id consistent with Ior getOrElse") {
    forAll { (iort: IorT[Id, String, Int], i: Int) =>
      assert(iort.getOrElseF(i) === iort.value.getOrElse(i))
    }
  }

  test("getOrRaise consistent with IorT.getOrElseF(F.raiseError(e))") {
    forAll { (iort: IorT[Either[String, *], String, Int], error: String) =>
      assertEquals(
        obtained = iort.getOrRaise(error),
        expected = iort.getOrElseF(Left(error))
      )
    }
  }

  test("valueOr with Id consistent with Ior valueOr") {
    forAll { (iort: IorT[Id, String, Int], f: String => Int) =>
      assert(iort.valueOr(f) === (iort.value.valueOr(f)))
    }
  }

  test("forall with Id consistent with Ior forall") {
    forAll { (iort: IorT[Id, String, Int], f: Int => Boolean) =>
      assert(iort.forall(f) === (iort.value.forall(f)))
    }
  }

  test("exists with Id consistent with Ior exists") {
    forAll { (iort: IorT[Id, String, Int], f: Int => Boolean) =>
      assert(iort.exists(f) === (iort.value.exists(f)))
    }
  }

  test("toOption consistent with isLeft") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.toOption.isDefined.map(!_) === (iort.isLeft))
    }
  }

  test("toEither consistent with toOption") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.toEither.toOption === (iort.toOption))
    }
  }

  test("toEither consistent with isLeft") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.toEither.isLeft === (iort.isLeft))
    }
  }

  test("toNested has no loss") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.toNested.value === (iort.value))
    }
  }

  test("toNestedValidated consistent with Ior toValidated") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.toNestedValidated.value === (iort.value.map(_.toValidated)))
    }
  }

  test("toValidated consistent with Ior toValidated") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.toValidated === (iort.value.map(_.toValidated)))
    }
  }

  test("to consistent with toOption") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.to[Option] === (iort.toOption.value))
    }
  }

  test("collectRight with List consistent with flattening a to[List]") {
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.collectRight === (iort.to[List].flatten))
    }
  }

  test("merge with Id consistent with Ior merge") {
    forAll { (iort: IorT[Id, Int, Int]) =>
      assert(iort.merge === (iort.value.merge))
    }
  }

  test("mapK consistent with f(value)+pure") {
    val f: List ~> Option = new (List ~> Option) { def apply[A](a: List[A]): Option[A] = a.headOption }
    forAll { (iort: IorT[List, String, Int]) =>
      assert(iort.mapK(f) === (IorT(f(iort.value))))
    }
  }

  test("leftMap with Id consistent with Ior leftMap") {
    forAll { (iort: IorT[Id, String, Int], f: String => Long) =>
      assert(iort.leftMap(f).value === (iort.value.leftMap(f)))
    }
  }

  test("leftFlatMap consistent with leftMap") {
    forAll { (iort: IorT[List, String, Int], f: String => String) =>
      assert(iort.leftFlatMap(v => IorT.left[Int](List(f(v)))) === (iort.leftMap(f)))
    }
  }

  test("leftFlatMap consistent with swap and then flatMap") {
    forAll { (iort: IorT[List, String, Int], f: String => IorT[List, String, Int]) =>
      assert(iort.leftFlatMap(f) === (iort.swap.flatMap(a => f(a).swap).swap))
    }
  }

  test("leftSemiflatMap consistent with leftMap") {
    forAll { (iort: IorT[List, String, Int], f: String => String) =>
      assert(iort.leftSemiflatMap(v => List(f(v))) === (iort.leftMap(f)))
    }
  }

  test("leftSemiflatmap consistent with swap and the semiflatMap") {
    forAll { (iort: IorT[List, String, Int], f: String => List[String]) =>
      assert(iort.leftSemiflatMap(f) === (iort.swap.semiflatMap(a => f(a)).swap))
    }
  }

  test("transform consistent with value.map") {
    forAll { (iort: IorT[List, String, Int], f: Ior[String, Int] => Ior[Long, Double]) =>
      assert(iort.transform(f) === (IorT(iort.value.map(f))))
    }
  }

  test("applyAlt with Id consistent with map") {
    forAll { (iort: IorT[Id, String, Int], f: Int => String) =>
      assert(iort.applyAlt(IorT.pure(f)) === (iort.map(f)))
    }
  }

  test("flatMapF consistent with flatMap") {
    forAll { (iort: IorT[List, String, Int], f: Int => IorT[List, String, Int]) =>
      assert(iort.flatMapF(f(_).value) === (iort.flatMap(f)))
    }
  }

  test("subflatMap consistent with value.map+flatMap") {
    forAll { (iort: IorT[List, String, Int], f: Int => Ior[String, Double]) =>
      assert(iort.subflatMap(f) === (IorT(iort.value.map(_.flatMap(f)))))
    }
  }

  test("semiflatMap consistent with value.flatMap+f+right/both") {
    forAll { (iort: IorT[List, String, Int], f: Int => List[Long]) =>
      assert(iort.semiflatMap(f) === IorT(iort.value.flatMap {
        case l @ Ior.Left(_) => List(l.asInstanceOf[Ior[String, Long]])
        case Ior.Right(b)    => f(b).map(Ior.right)
        case Ior.Both(a, b)  => f(b).map(Ior.both(a, _))
      }))
    }
  }

  test("IorT.left with Option isLeft") {
    forAll { (option: Option[String]) =>
      assert(IorT.left[Int](option).isLeft === (option.map(_ => true)))
    }
  }

  test("IorT.leftT isLeft") {
    forAll { (s: String) =>
      assert(IorT.leftT[Option, Int](s).isLeft === (Some(true)))
    }
  }

  test("IorT.right with Option isRight") {
    forAll { (option: Option[Int]) =>
      assert(IorT.right[String](option).isRight === (option.map(_ => true)))
    }
  }

  test("IorT.rightT consistent with IorT.pure") {
    forAll { (i: Int) =>
      assert(IorT.rightT[Option, String](i).value === (IorT.pure[Option, String](i).value))
    }
  }

  test("IorT.both isBoth with Option consistent with Option zip") {
    forAll { (optionS: Option[String], optionI: Option[Int]) =>
      assert(IorT.both(optionS, optionI).isBoth === (optionS.zip(optionI).headOption.map(_ => true)))
    }
  }

  test("IorT.bothT isBoth") {
    forAll { (s: String, i: Int) =>
      assert(IorT.bothT[Option](s, i).isBoth === (Some(true)))
    }
  }

  test("IorT.pure isRight") {
    forAll { (i: Int) =>
      assert(IorT.rightT[Option, String](i).isRight === (Some(true)))
    }
  }

  test("IorT.liftF consistent with IorT.right") {
    forAll { (option: Option[Int]) =>
      assert(IorT.liftF[Option, String, Int](option).value === (IorT.right[String](option).value))
    }
  }

  test("IorT.fromIor with Id is noop") {
    forAll { (ior: Ior[String, Int]) =>
      assert(IorT.fromIor[Id](ior).value === ior)
    }
  }

  test("IorT.fromEither toEither is noop") {
    forAll { (either: Either[String, Int]) =>
      assert(IorT.fromEither[Id](either).value.toEither === either)
    }
  }

  test("IorT.fromEitherF toEither is noop") {
    forAll { (either: Either[String, Int]) =>
      assert(IorT.fromEitherF[Id, String, Int](either).value.toEither === either)
    }
  }

  test("IorT.fromOption isLeft consistent with Option isEmpty") {
    forAll { (option: Option[Int], s: String) =>
      assert(IorT.fromOption[Id](option, s).isLeft === (option.isEmpty))
    }
  }

  test("IorT.fromOptionF isLeft consistent with Option isEmpty") {
    forAll { (option: Option[Int], s: String) =>
      assert(IorT.fromOptionF[Id, String, Int](option, s).isLeft === (option.isEmpty))
    }
  }

  test("IorT.fromOptionM consistent with IorT.fromOptionF") {
    forAll { (option: Option[Int], s: String) =>
      assert(IorT.fromOptionM[Id, String, Int](option, s) === (IorT.fromOptionF[Id, String, Int](option, s)))
    }
  }

  test("IorT.cond isRight equals test") {
    forAll { (test: Boolean, s: String, i: Int) =>
      val iort = IorT.cond[Id](test, s, i)
      assert((iort.isRight && !iort.isLeft && !iort.isBoth) === test)
    }
  }

  test("IorT.condF consistent with IorT.right and IorT.left") {
    forAll { (test: Boolean, optionS: Option[String], optionI: Option[Int]) =>
      IorT.condF(test, optionS, optionI) === (if (test) IorT.right(optionS) else IorT.left(optionI))
    }
  }
}
