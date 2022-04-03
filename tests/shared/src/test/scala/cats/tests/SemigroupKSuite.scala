package cats.tests

import cats.{Align, Alternative, SemigroupK}
import cats.data.{Chain, Validated}
import cats.laws.discipline.AlignTests
import cats.laws.discipline.arbitrary._

class SemigroupKSuite extends CatsSuite {
  implicit val listwrapperSemigroupK: Alternative[ListWrapper] = ListWrapper.alternative
  implicit val listwrapperAlign: Align[ListWrapper] = SemigroupK.align[ListWrapper]
  checkAll("SemigroupK[ListWrapper].align", AlignTests[ListWrapper].align[Int, Int, Int, Int])

  implicit val validatedAlign: Align[Validated[String, *]] = SemigroupK.align[Validated[String, *]]
  checkAll("SemigroupK[Validated].align", AlignTests[Validated[String, *]].align[Int, Int, Int, Int])

  implicit val chainAlign: Align[Chain] = SemigroupK.align[Chain]
  checkAll("SemigroupK[Chain].align", AlignTests[Chain].align[Int, Int, Int, Int])
}
