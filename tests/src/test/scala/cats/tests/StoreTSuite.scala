package cats.tests

import cats._
import cats.data.{StoreT, Validated}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class StoreTSuite extends CatsSuite {

  implicit val monoid: Monoid[MiniInt] = MiniInt.miniIntAddition

  checkAll("StoreT[Id, MiniInt, *]", ComonadTests[StoreT[Id, MiniInt, *]].comonad[MiniInt, MiniInt, MiniInt])

  checkAll("StoreT[Validated[String, *], MiniInt, *]",
           ApplicativeTests[StoreT[Validated[String, *], MiniInt, *]].applicative[MiniInt, MiniInt, MiniInt]
  )

}
