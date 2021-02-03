package cats.kernel.instances

import cats.kernel.{CommutativeGroup, Eq, Hash, Order}
import java.time._

trait JavaTimeInstances {

  implicit final val catsKernelStdOrderForduration
    : Hash[Duration] with Order[Duration] with CommutativeGroup[Duration] =
    new Order.FromComparable[Duration] with Hash.FromUniversal[Duration] with CommutativeGroup[Duration] {
      override def empty: Duration = Duration.ZERO
      override def combine(x: Duration, y: Duration): Duration = x.plus(y)
      override def inverse(x: Duration): Duration = x.negated()
    }

  implicit final val catsKernelStdOrderForInstant: Order[Instant] with Hash[Instant] =
    new Order.FromComparable[Instant] with Hash.FromUniversal[Instant] {}

  implicit final val catsKernelStdEqForLocalDate: Eq[LocalDate] with Hash[LocalDate] =
    new Eq.FromUniversal[LocalDate] with Hash.FromUniversal[LocalDate] {}

  implicit final val catsKernelStdEqForLocalDateTime: Eq[LocalDateTime] with Hash[LocalDateTime] =
    new Eq.FromUniversal[LocalDateTime] with Hash.FromUniversal[LocalDateTime] {}

  implicit final val catsKernelStdOrderForLocalTime: Order[LocalTime] with Hash[LocalTime] =
    new Order.FromComparable[LocalTime] with Hash.FromUniversal[LocalTime] {}

  implicit final val catsKernelStdOrderForMonth: Order[Month] with Hash[Month] =
    new Order.FromComparable[Month] with Hash.FromUniversal[Month] {}

  implicit final val catsKernelStdOrderForMonthDay: Order[MonthDay] with Hash[MonthDay] =
    new Order.FromComparable[MonthDay] with Hash.FromUniversal[MonthDay] {}

  implicit final val catsKernelStdOrderForOffsetDateTime: Order[OffsetDateTime] with Hash[OffsetDateTime] =
    new Order.FromComparable[OffsetDateTime] with Hash.FromUniversal[OffsetDateTime] {}

  implicit final val catsKernelStdOrderForOffsetTime: Order[OffsetTime] with Hash[OffsetTime] =
    new Order.FromComparable[OffsetTime] with Hash.FromUniversal[OffsetTime] {}

  implicit final val catsKernelStdOrderForYear: Order[Year] with Hash[Year] =
    new Order.FromComparable[Year] with Hash.FromUniversal[Year] {}

  implicit final val catsKernelStdOrderForYearMonth: Order[YearMonth] with Hash[YearMonth] =
    new Order.FromComparable[YearMonth] with Hash.FromUniversal[YearMonth] {}

  implicit final val catsKernelStdEqForZoneId: Eq[ZoneId] with Hash[ZoneId] =
    new Eq.FromUniversal[ZoneId] with Hash.FromUniversal[ZoneId] {}

  implicit final val catsKernelStdOrderForZoneOffset: Order[ZoneOffset] with Hash[ZoneOffset] =
    new Order.FromComparable[ZoneOffset] with Hash.FromUniversal[ZoneOffset] {}

  implicit final val catsKernelStdOrderForZonedDateTime: Eq[ZonedDateTime] with Hash[ZonedDateTime] =
    new Eq.FromUniversal[ZonedDateTime] with Hash.FromUniversal[ZonedDateTime] {}

}
