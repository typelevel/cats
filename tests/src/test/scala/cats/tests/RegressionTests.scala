package cats
package tests

import cats.data.{Const, NonEmptyList, Xor}

import scala.collection.mutable

class RegressionTests extends CatsSuite {

  // toy state class
  // not stack safe, very minimal, not for actual use
  case class State[S, A](run: S => (A, S)) { self =>
    def map[B](f: A => B): State[S, B] =
      State({ s => val (a, s2) = self.run(s); (f(a), s2) })
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State({ s => val (a, s2) = self.run(s); f(a).run(s2) })
  }

  object State {
    implicit def instance[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {
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
    Traverse[List].sequence(ons) should === (Some(List(1, 2, 3)))

    // test order of effects using a contrived, unsafe state monad.
    val names = List("Alice", "Bob", "Claire")
    val allocated = names.map(alloc)
    val state = Traverse[List].sequence[State[Int, ?],Person](allocated)
    val (people, counter) = state.run(0)
    people should === (List(Person(0, "Alice"), Person(1, "Bob"), Person(2, "Claire")))
    counter should === (3)

    // ensure that side-effects occurred in "correct" order
    buf.toList should === (names)
  }

  test("#167: confirm ap2 order") {
    val twelve = Apply[State[String, ?]].ap2(State.instance[String].pure((_: Unit, _: Unit) => ()))(
      State[String, Unit](s => ((), s + "1")),
      State[String, Unit](s => ((), s + "2"))
    ).run("")._2
    twelve should === ("12")
  }

  test("#167: confirm map2 order") {
    val twelve = Apply[State[String, ?]].map2(
      State[String, Unit](s => ((), s + "1")),
      State[String, Unit](s => ((), s + "2"))
    )((_: Unit, _: Unit) => ()).run("")._2
    twelve should === ("12")
  }

  test("#167: confirm map3 order") {
    val oneTwoThree = Apply[State[String, ?]].map3(
      State[String, Unit](s => ((), s + "1")),
      State[String, Unit](s => ((), s + "2")),
      State[String, Unit](s => ((), s + "3"))
    )((_: Unit, _: Unit, _: Unit) => ()).run("")._2
    oneTwoThree should === ("123")
  }

  test("#500: foldMap - traverse consistency") {
    assert(
      List(1,2,3).traverseU(i => Const(List(i))).getConst == List(1,2,3).foldMap(List(_))
    )
  }

  test("#513: traverse short circuits - Xor") {
    var count = 0
    def validate(i: Int): Xor[String, Int] = {
      count = count + 1
      if (i < 5) Xor.right(i) else Xor.left(s"$i is greater than 5")
    }

    def checkAndResetCount(expected: Int): Unit = {
      count should === (expected)
      count = 0
    }

    List(1,2,6,8).traverseU(validate) should === (Xor.left("6 is greater than 5"))
    // shouldn't have ever evaluted validate(8)
    checkAndResetCount(3)

    Stream(1,2,6,8).traverseU(validate) should === (Xor.left("6 is greater than 5"))
    checkAndResetCount(3)

    type StringMap[A] = Map[String, A]
    val intMap: StringMap[Int] = Map("one" -> 1, "two" -> 2, "six" -> 6, "eight" -> 8)
    intMap.traverseU(validate) should === (Xor.left("6 is greater than 5"))
    checkAndResetCount(3)

    NonEmptyList.of(1,2,6,8).traverseU(validate) should === (Xor.left("6 is greater than 5"))
    checkAndResetCount(3)

    NonEmptyList.of(6,8).traverseU(validate) should === (Xor.left("6 is greater than 5"))
    checkAndResetCount(1)

    List(1,2,6,8).traverseU_(validate) should === (Xor.left("6 is greater than 5"))
    checkAndResetCount(3)

    NonEmptyList.of(1,2,6,7,8).traverseU_(validate) should === (Xor.left("6 is greater than 5"))
    checkAndResetCount(3)

    NonEmptyList.of(6,7,8).traverseU_(validate) should === (Xor.left("6 is greater than 5"))
    checkAndResetCount(1)
  }
}
