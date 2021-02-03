package cats.kernel
package laws

import cats.kernel.instances.time._
import cats.kernel.laws.discipline._
import java.time._
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}

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

  checkAll(
    "Monoid[Duration]", {
      implicit val arbitraryFiniteDuration: Arbitrary[Duration] = {
        // max range is +/- 292 years, but we give ourselves some extra headroom
        // to ensure that we can add these things up. they crash on overflow.
        val n = (292L * 365) / 500
        Arbitrary(
          Gen.oneOf(
            Gen.choose(-n, n).map(Duration.ofDays),
            Gen.choose(-n * 24L, n * 24L).map(Duration.ofHours),
            Gen.choose(-n * 1440L, n * 1440L).map(Duration.ofMinutes),
            Gen.choose(-n * 86400L, n * 86400L).map(Duration.ofSeconds),
            Gen.choose(-n * 86400000L, n * 86400000L).map(Duration.ofMillis),
            Gen.choose(-n * 86400000000000L, n * 86400000000000L).map(Duration.ofNanos)
          )
        )
      }

      // For this tests we use a duration range of +/- 100 years,
      CommutativeGroupTests[Duration].commutativeGroup
    }
  )

}
