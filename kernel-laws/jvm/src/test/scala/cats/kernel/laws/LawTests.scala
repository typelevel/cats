package cats.kernel
package laws

import cats.kernel.instances.time._
import cats.kernel.laws.discipline._
import java.time._
import munit.DisciplineSuite

class JvmTests extends TestsConfig with DisciplineSuite {

  // Java Time instances
  checkAll("Eq[LocalDate]", EqTests[LocalDateTime].eqv)
  checkAll("Eq[LocalDateTime]", EqTests[LocalDateTime].eqv)
  checkAll("Eq[ZoneId]", EqTests[ZoneId].eqv)
  checkAll("Eq[ZonedDateTime]", EqTests[ZonedDateTime].eqv)
  checkAll("Order[java.time.Duration]", OrderTests[Duration].order)
  checkAll("Order[Instant]", OrderTests[Instant].order)
  checkAll("Order[LocalTime]", OrderTests[LocalTime].order)
  checkAll("Order[Month]", OrderTests[Month].order)
  checkAll("Order[MonthDay]", OrderTests[MonthDay].order)
  checkAll("Order[OffsetDateTime]", OrderTests[OffsetDateTime].order)
  checkAll("Order[OffsetTime]", OrderTests[OffsetTime].order)
  checkAll("Order[Year]", OrderTests[Year].order)
  checkAll("Order[YearMonth]", OrderTests[YearMonth].order)
  checkAll("Order[ZoneOffset]", OrderTests[ZoneOffset].order)

  checkAll("Hash[java.time.Duration]", HashTests[java.time.Duration].hash)
  checkAll("Hash[Instant]", HashTests[Instant].hash)
  checkAll("Hash[LocalDate]", HashTests[LocalDate].hash)
  checkAll("Hash[LocalDateTime]", HashTests[LocalDateTime].hash)
  checkAll("Hash[LocalTime]", HashTests[LocalTime].hash)
  checkAll("Hash[Month]", HashTests[Month].hash)
  checkAll("Hash[MonthDay]", HashTests[MonthDay].hash)
  checkAll("Hash[OffsetDateTime]", HashTests[OffsetDateTime].hash)
  checkAll("Hash[OffsetTime]", HashTests[OffsetTime].hash)
  checkAll("Hash[Year]", HashTests[Year].hash)
  checkAll("Hash[YearMonth]", HashTests[YearMonth].hash)
  checkAll("Hash[ZoneId]", HashTests[ZoneId].hash)
  checkAll("Hash[ZoneOffset]", HashTests[ZoneOffset].hash)
  checkAll("Hash[ZonedDateTime]", HashTests[ZonedDateTime].hash)

}
