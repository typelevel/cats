package cats.instances

import cats.Show
import java.time._

trait JavaTimeInstances extends cats.kernel.instances.JavaTimeInstances {

  implicit final val catsCoreStdShowForInstant: Show[Instant] = Show.fromToString
  implicit final val catsCoreStdShowForLocalDate: Show[LocalDate] = Show.fromToString
  implicit final val catsCoreStdShowForLocalDateTime: Show[LocalDateTime] = Show.fromToString
  implicit final val catsCoreStdShowForLocalTime: Show[LocalTime] = Show.fromToString
  implicit final val catsCoreStdShowForMonth: Show[Month] = Show.fromToString
  implicit final val catsCoreStdShowForMonthDay: Show[MonthDay] = Show.fromToString
  implicit final val catsCoreStdShowForOffsetDateTime: Show[OffsetDateTime] = Show.fromToString
  implicit final val catsCoreStdShowForOffsetTime: Show[OffsetTime] = Show.fromToString
  implicit final val catsCoreStdShowForYear: Show[Year] = Show.fromToString
  implicit final val catsCoreStdShowForYearMonth: Show[YearMonth] = Show.fromToString
  implicit final val catsCoreStdShowForZoneId: Show[ZoneId] = Show.fromToString
  implicit final val catsCoreStdShowForZoneOffset: Show[ZoneOffset] = Show.fromToString
  implicit final val catsCoreStdShowForZonedDateTime: Show[ZonedDateTime] = Show.fromToString

}
