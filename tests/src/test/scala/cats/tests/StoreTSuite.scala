package cats.tests

import cats._
import cats.data.StoreT
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary

class StoreTSuite extends CatsSuite {

  checkAll("StoreT[Id, String, *]", ComonadTests[StoreT[Id, MiniInt, *]].comonad[MiniInt, MiniInt, MiniInt])

}
