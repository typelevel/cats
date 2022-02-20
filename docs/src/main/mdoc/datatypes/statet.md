---
layout: docs
title:  "StateT"
section: "data"
source: "core/src/main/scala/cats/data/package.scala"
scaladoc: "#cats.data.StateT"
---
# StateT

`StateT[F[_], S, A]` is a data type used to define state machine
effectful computations that can be easily composed into a larger one.

The type parameters are:
- `F[_]` represents the effect in which the computation is performed.
- `S` represents the underlying state, shared between each step of the
  state machine.
- `A` represents the return value.

It can be seen as a way to add the capability to manipulate a shared
state to an existing computation in the context of `F`.

This definition could be confusing, but it will become clear after
learning the `State` monad and by the example below.

## StateT and State Relationship

`StateT` is a monad transformer for
[State](https://typelevel.org/cats/datatypes/state.html), in
particular [State](https://typelevel.org/cats/datatypes/state.html) is
defined as a `StateT` with
[Eval](https://typelevel.org/cats/datatypes/eval.html) as the effect
`F`.

```scala mdoc:silent
import cats.data.StateT
import cats.Eval

type State[S, A] = StateT[Eval, S, A]
```

Therefore, `StateT` exposes the same methods of
[State](https://typelevel.org/cats/datatypes/state.html), such as:
`modify`, `get` and `set`.  Plus additional methods, that handles
effectful computations, eg: `modifyF`, `setF` and `liftF`.

## Example: Table Reservation System

In this example, we are going to model a *Table Reservation System*. To
do so, we need to keep track of the tables and the current
reservations for each of them. The end-user can then try to insert a
booking for a specific table and time. If such a table is available,
then the booking is placed and the state is updated, otherwise, an
error is returned.

To simplify the logic, for each reservation we will just consider a
single `LocalTime` starting at the beginning of the hour.

Let's start with defining the type synonym for the effect type:

```scala mdoc:silent:reset
import cats.data.{StateT, NonEmptyList}
import cats.implicits._
import java.time.LocalTime

type ThrowableOr[A] = Either[Throwable, A]
```

We will use an `Either[Throwable, A]` because each
booking insertion can fail.

Now, we need to implement/define:
- The type representing the reservation.
- The type representing the state of the Table Reservation System. It
  will wrap around a collection of Reservations.
- An initial state, that will be just an empty state type.
- A custom `Throwable` to be used in case of an error.
- The logic for the booking insertion. We can take advantage of the
method `modifyF` later on to apply it to the system state.

In addition, we can implement a simple function that will evaluate a
`NonEmptyList` of reservations, inserting them one by one.

```scala mdoc:silent
object TableReservationSystem {

  final case class ReservationId(tableNumber: Int, hour: LocalTime)
  final case class Reservation(id: ReservationId, name: String)
  final case class Reservations(reservations: List[Reservation]) {
    def insert(reservation: Reservation): ThrowableOr[Reservations] =
      if (reservations.exists(r => r.id == reservation.id))
        Left(TableAlreadyReserved(reservation))
      else Right(Reservations(reservations :+ reservation))
  }

  final case class TableAlreadyReserved(
      reservation: Reservation
  ) extends RuntimeException(
        s"${reservation.name} cannot be added because table number ${reservation.id.tableNumber} is already reserved for the ${reservation.id.hour}"
      )

  val emptyReservationSystem: Reservations = Reservations(List.empty)

  def insertBooking(
      reservation: Reservation
  ): StateT[ThrowableOr, Reservations, Unit] =
    StateT.modifyF[ThrowableOr, Reservations](_.insert(reservation))

  def evalBookings(
      bookings: NonEmptyList[Reservation]
  ): ThrowableOr[Reservations] =
    bookings
      .map(insertBooking)
      .reduceLeft[StateT[ThrowableOr, Reservations, Unit]] {
        case (acc, bookNext) => acc.productL(bookNext)
      }
      .runS(emptyReservationSystem)
}
```

That's it, we can finally test the code above providing a simple
example input and print out the result.

```scala mdoc
val bookings = NonEmptyList.of(
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 1, hour = LocalTime.parse("10:00:00")),
    name = "Cristiano Ronaldo"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 2, hour = LocalTime.parse("10:00:00")),
    name = "Lebron James"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 1, hour = LocalTime.parse("12:00:00")),
    name = "Tiger Woods"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 2, hour = LocalTime.parse("12:00:00")),
    name = "Roger Federer"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 3, hour = LocalTime.parse("13:00:00")),
    name = "Elizabeth Alexandra Mary"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 1, hour = LocalTime.parse("16:00:00")),
    name = "Chuck Norris"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 2, hour = LocalTime.parse("16:00:00")),
    name = "Kobe Bryant"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 2, hour = LocalTime.parse("18:00:00")),
    name = "Steven Seagal"
  )
)

TableReservationSystem.evalBookings(bookings)

TableReservationSystem.evalBookings(
  bookings :+ TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 1, hour = LocalTime.parse("16:00:00")),
    name = "Bruce Lee"
  )
)
```

The full source code of this example can be found at this
[gist](https://gist.github.com/benkio/baa4fe1d50751cd602c4175f1bb39f4d)
or [scastie](https://scastie.scala-lang.org/WmnSoQFyS7SIGxgly3M6QQ)

## Example: Hangman Game

This example is a little bit more complex since it involves more
steps, checks and I/O operations. Here we need to:
- Keep a state of the game, with the target word and the guesses made.
- Show the game state.
- Ask the player to input a guess word until the game is over.

The whole game state machine can be summarized as follows:

```asciidoc

    +----------------------+                 +----------------------+                   +-------------------+
    | Ask for Target Word  +---------------->| Show the word hidden |------------------>| Ask for the guess |
    +----------------------+                 +----------------------+                   +-------------------+
                                                    ^                                             |
                                                    |                                             |
                                                    |                                             |
                  +-----------+          +---------------+       +-----------------------+        |
                  | Game Over |<---------+ Check the win |<------| Compute the new state |<-------+
                  +-----------+          |   condition   |       +-----------------------+
                                         +---------------+

```

At each step, the state might be updated and, since some I/O
operations need to be performed along the way, a specific effect (`IO`
monad) has to be used.

We can model the game state as follow:

```scala mdoc:silent:reset
import cats.data.StateT
import cats.Eval
import scala.io.StdIn._

final case class GameState(
    target: String,
    guessedChars: Set[Char],
    attemptNum: Int
)

sealed trait GameOver
case object Win extends GameOver
case object Loose extends GameOver

object GameState {

  val maxAttempts: Int = 3

  def apply(target: String): GameState = GameState(
    target = target,
    guessedChars = Set.empty[Char],
    attemptNum = 0
  )

  def showWordHidden(state: GameState): String =
    state.target.map(c => if (state.guessedChars.contains(c)) c else '*')

  def attemptGuess(
      state: GameState,
      guess: String
  ): Either[GameOver, GameState] =
    state match {
      case GameState(t, _, _) if t == guess => Left(Win)
      case GameState(_, _, attemptNum) if attemptNum == (maxAttempts - 1) =>
        Left(Loose)
      case GameState(target, guessedChars, attemptNum) =>
        Right(
          GameState(
            target = target,
            guessedChars = guessedChars ++ guess,
            attemptNum = attemptNum + 1
          )
        )
    }
}
```

In the code above you can see some useful functions, such as the one
that returns the encrypted target word.

In the following snippet you can see the above code in action:

```scala mdoc
val gameState1 = GameState("cats")
GameState.showWordHidden(gameState1)
val gameState2 = GameState.attemptGuess(gameState1, "bats")
GameState.showWordHidden(gameState2.getOrElse(???))
```

Unfortunately, the rest of the example can't be shown here due to the
required `cats-effect` dependency. We recommend to check out the rest
of the code at the following
[gist](https://gist.github.com/benkio/46f5aea4f15ec059f02d6bfe9bd25e99)
or [scastie](https://scastie.scala-lang.org/4Ab7xspkRJ2q9UKQ9OHrUQ).

We hope these examples help to clarify how `StateT` can be used in
designing a computation based on state machine steps that may require
side effects or other capabilities, eg `Option`, `List`, `Fiber` etc.
