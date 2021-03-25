package cats.tests

import cats._
import cats.data.{StoreT, Validated}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._

class StoreTSuite extends CatsSuite {

  implicit val monoid: Monoid[MiniInt] = MiniInt.miniIntAddition

  checkAll("StoreT[Id, MiniInt, *]", ComonadTests[StoreT[Id, MiniInt, *]].comonad[MiniInt, MiniInt, MiniInt])

  checkAll("Comonad[StoreT[Id, MiniInt, *]]", SerializableTests.serializable(Comonad[StoreT[Id, MiniInt, *]]))

  checkAll("StoreT[Validated[String, *], MiniInt, *]]",
           ApplicativeTests[StoreT[Validated[String, *], MiniInt, *]].applicative[MiniInt, MiniInt, MiniInt]
  )

  checkAll("Comonad[StoreT[Validated[String, *], MiniInt, *]]",
           SerializableTests.serializable(Applicative[StoreT[Validated[String, *], MiniInt, *]])
  )

}
