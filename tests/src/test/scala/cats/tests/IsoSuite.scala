package cats.tests

import cats.{Iso, Semigroup}
import cats.Iso.{<=>, HasIso}

class IsoSuite extends CatsSuite {

  implicit val isoSI: String <=> Int = Iso.unsafe(_.toInt, _.toString)
  implicit val isoIL: Int <=> Long = Iso.unsafe(_.toLong, _.toInt)

  test("HasIso implicit search") {
    implicitly[HasIso[* => *, String, Int]]
    implicitly[HasIso[* => *, Int, String]]
    implicitly[HasIso[* => *, Int, Int]]
  }

  test("iso teleport") {
    isoSI.teleport(i => i * 2)("40") === "80"
  }

  test("iso derive") {
    val sl: Semigroup[Long] = isoIL.derive[Semigroup]
    sl.combine(1L, 2L) === 3L
  }

  test("iso chain") {
    isoSI.chain[Long].flip.chain[String].chain[String].apply(4L) === "4"
  }

}
