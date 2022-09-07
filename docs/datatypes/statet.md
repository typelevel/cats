# StateT

`StateT[F[_], S, A]` is a data type that generalizes `State` with the
ability to compose with effects in `F[_]`. Because `StateT` is defined
in terms of `F`, it is a monad only if `F` is a monad. Additionally,
`StateT` may acquire new capabilities via `F`: for example, if `F` is
capable of error handling via `MonadThrow[F]`, then Cats derives an
instance of `MonadThrow[StateT[F, S, *]]`.

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

Let's start with defining the type alias for the effect type:

```scala mdoc:silent:reset
import cats.data.{StateT, NonEmptyList}
import cats.syntax.all._
import java.time.LocalTime

type ThrowableOr[A] = Either[Throwable, A]
```

We will use an `Either[Throwable, A]` to model either success `Right(a)` or failure `Left(ex)`.

Now, we need to implement/define:
- The type representing the reservation.
- The type representing the state of the Table Reservation System. It
  will wrap around a collection of Reservations.
- An initial state, that will be just empty (no reservations).
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
        Left(new TableAlreadyReservedException(reservation))
      else Right(Reservations(reservations :+ reservation))
  }

  final class TableAlreadyReservedException(
      reservation: Reservation
  ) extends RuntimeException(
        s"${reservation.name} cannot be added because table number ${reservation.id.tableNumber} is already reserved for the ${reservation.id.hour}"
      )

  val emptyReservationSystem: Reservations = Reservations(List.empty)

  def insertBooking(
      reservation: Reservation
  ): StateT[ThrowableOr, Reservations, Unit] =
    StateT.modifyF[ThrowableOr, Reservations](_.insert(reservation))

  def processBookings(
      bookings: NonEmptyList[Reservation]
  ): ThrowableOr[Reservations] =
    bookings
      .traverse_(insertBooking)
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
    name = "Gandalf"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 2, hour = LocalTime.parse("10:00:00")),
    name = "Legolas"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 1, hour = LocalTime.parse("12:00:00")),
    name = "Frodo"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 2, hour = LocalTime.parse("12:00:00")),
    name = "Bilbo"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 3, hour = LocalTime.parse("13:00:00")),
    name = "Elrond"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 1, hour = LocalTime.parse("16:00:00")),
    name = "Sauron"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 2, hour = LocalTime.parse("16:00:00")),
    name = "Aragorn"
  ),
  TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 2, hour = LocalTime.parse("18:00:00")),
    name = "Gollum"
  )
)

TableReservationSystem.processBookings(bookings)

TableReservationSystem.processBookings(
  bookings :+ TableReservationSystem.Reservation(
    TableReservationSystem
      .ReservationId(tableNumber = 1, hour = LocalTime.parse("16:00:00")),
    name = "Saruman"
  )
)
```

The full source code of this example can be found at this
[gist](https://gist.github.com/benkio/baa4fe1d50751cd602c4175f1bb39f4d)
or [scastie](https://scastie.scala-lang.org/YhJmET6PRJKvrZTfrZJkbQ)
