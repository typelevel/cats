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
import cats.data.Const
import cats.data.NonEmptyList
import cats.data.StateT
import cats.kernel.compat.scalaVersionSpecific.*
import cats.syntax.all.*

import scala.collection.immutable.SortedMap
import scala.collection.mutable

@suppressUnusedImportWarningForScalaVersionSpecific
class RegressionSuite extends CatsSuite with ScalaVersionSpecificRegressionSuite {

  // toy state class
  // not stack safe, very minimal, not for actual use
  case class State[S, A](run: S => (A, S)) { self =>
    def map[B](f: A => B): State[S, B] =
      State { s =>
        val (a, s2) = self.run(s); (f(a), s2)
      }
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { s =>
        val (a, s2) = self.run(s); f(a).run(s2)
      }
  }

  object State {
    implicit def instance[S]: Monad[State[S, *]] =
      new Monad[State[S, *]] with StackSafeMonad[State[S, *]] { // lies!
        def pure[A](a: A): State[S, A] = State(s => (a, s))
        def flatMap[A, B](sa: State[S, A])(f: A => State[S, B]): State[S, B] = sa.flatMap(f)
      }
  }

  // used to test side-effects
  val buf = mutable.ListBuffer.empty[String]

  case class Person(id: Int, name: String)
  implicit val personEq: Eq[Person] = Eq.fromUniversalEquals

  def alloc(name: String): State[Int, Person] =
    State { id =>
      buf.append(name)
      (Person(id, name), id + 1)
    }

  test("#140: confirm sequence order") {

    // test result order
    val ons = List(Option(1), Option(2), Option(3))
    assert(Traverse[List].sequence(ons) === (Some(List(1, 2, 3))))

    // test order of effects using a contrived, unsafe state monad.
    val names = List("Alice", "Bob", "Claire")
    val allocated = names.map(alloc)
    val state = Traverse[List].sequence[State[Int, *], Person](allocated)
    val (people, counter) = state.run(0)
    assert(people === (List(Person(0, "Alice"), Person(1, "Bob"), Person(2, "Claire"))))
    assert(counter === 3)

    // ensure that side-effects occurred in "correct" order
    assert(buf.toList === names)
  }

  test("#167: confirm ap2 order") {
    val twelve = Apply[State[String, *]]
      .ap2(State.instance[String].pure((_: Unit, _: Unit) => ()))(
        State[String, Unit](s => ((), s + "1")),
        State[String, Unit](s => ((), s + "2"))
      )
      .run("")
      ._2
    assert(twelve === "12")
  }

  test("#167: confirm map2 order") {
    val twelve = Apply[State[String, *]]
      .map2(
        State[String, Unit](s => ((), s + "1")),
        State[String, Unit](s => ((), s + "2"))
      )((_: Unit, _: Unit) => ())
      .run("")
      ._2
    assert(twelve === "12")
  }

  test("#167: confirm map3 order") {
    val oneTwoThree = Apply[State[String, *]]
      .map3(
        State[String, Unit](s => ((), s + "1")),
        State[String, Unit](s => ((), s + "2")),
        State[String, Unit](s => ((), s + "3"))
      )((_: Unit, _: Unit, _: Unit) => ())
      .run("")
      ._2
    assert(oneTwoThree === "123")
  }

  test("#500: foldMap - traverse consistency") {
    assert(
      List(1, 2, 3).traverse(i => Const[List[Int], List[Int]](List(i))).getConst == List(1, 2, 3).foldMap(List(_))
    )
  }

  test("#513: traverse short circuits - Either") {
    var count = 0
    def validate(i: Int): Either[String, Int] = {
      count = count + 1
      if (i < 5) Either.right(i) else Either.left(s"$i is greater than 5")
    }

    def checkAndResetCount(expected: Int): Unit = {
      assert(count === expected)
      count = 0
    }

    assert(List(1, 2, 6, 8).traverse(validate) === (Either.left("6 is greater than 5")))
    // shouldn't have ever evaluated validate(8)
    checkAndResetCount(3)

    {
      @annotation.nowarn("cat=deprecation")
      val obtained = Stream(1, 2, 6, 8).traverse(validate).void
      assert(obtained === Either.left("6 is greater than 5"))
    }
    checkAndResetCount(3)

    type StringMap[A] = SortedMap[String, A]
    val intMap: StringMap[Int] = SortedMap("A" -> 1, "B" -> 2, "C" -> 6, "D" -> 8)
    assert(intMap.traverse(validate) === (Either.left("6 is greater than 5")))
    checkAndResetCount(3)

    assert(NonEmptyList.of(1, 2, 6, 8).traverse(validate) === (Either.left("6 is greater than 5")))
    checkAndResetCount(3)

    assert(NonEmptyList.of(6, 8).traverse(validate) === (Either.left("6 is greater than 5")))
    checkAndResetCount(1)

    assert(Vector(1, 2, 6, 8).traverse(validate) === (Either.left("6 is greater than 5")))
    checkAndResetCount(3)

    assert(List(1, 2, 6, 8).traverseVoid(validate) === Either.left("6 is greater than 5"))
    checkAndResetCount(3)

    {
      @annotation.nowarn("cat=deprecation")
      val obtained = Stream(1, 2, 6, 8).traverseVoid(validate)
      assert(obtained === Either.left("6 is greater than 5"))
    }
    checkAndResetCount(3)

    assert(Vector(1, 2, 6, 8).traverseVoid(validate) === Either.left("6 is greater than 5"))
    checkAndResetCount(3)

    assert(NonEmptyList.of(1, 2, 6, 7, 8).traverseVoid(validate) === Either.left("6 is greater than 5"))
    checkAndResetCount(3)

    assert(NonEmptyList.of(6, 7, 8).traverseVoid(validate) === Either.left("6 is greater than 5"))
    checkAndResetCount(1)
  }

  test("#2022 EitherT syntax no long works the old way") {
    import cats.data.*

    EitherT.right[String](Option(1)).handleErrorWith((_: String) => EitherT.pure(2))

    {
      MonadError[EitherT[Option, String, *], Unit]
    }

    {
      implicit val me: MonadError[EitherT[Option, String, *], Unit] =
        EitherT.catsDataMonadErrorFForEitherT[Option, Unit, String]
      EitherT.right[String](Option(1)).handleErrorWith((_: Unit) => EitherT.pure(2))
    }

  }

  test("#2809 MonadErrorOps.reject runs effects only once") {
    val program = StateT.modify[Either[Throwable, *], Int](_ + 1).reject { case _ if false => new Throwable }
    assert(program.runS(0).toOption === (Some(1)))
  }
}
