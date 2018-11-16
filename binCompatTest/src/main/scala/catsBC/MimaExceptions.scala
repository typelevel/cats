package catsBC
import cats.implicits._

object MimaExceptions {

  def headOption[A](list: List[A]): Option[A] = list.headOption

  import cats.arrow.FunctionK // needs to be imported because of a hygiene problem

  def isBinaryCompatible = (
      cats.Monad[cats.data.OptionT[List, ?]],
      cats.data.OptionT.catsDataTraverseForOptionT[List],
      cats.data.Kleisli.catsDataCommutativeArrowForKleisliId,
      cats.data.OptionT.catsDataMonoidKForOptionT[List],
      cats.data.OptionT.catsDataMonoidForOptionT[List, Int],
      cats.data.Kleisli.catsDataMonadForKleisliId[Int],
      cats.data.Kleisli.catsDataCommutativeArrowForKleisli[Option],
      cats.data.Kleisli.catsDataCommutativeFlatMapForKleisli[Option, Int],
      cats.data.IRWST.catsDataStrongForIRWST[List, Int, Int, Int],
      cats.data.OptionT.catsDataMonadErrorMonadForOptionT[List],
      FunctionK.lift(headOption),
      cats.data.OptionT.catsDataMonadErrorForOptionT[Either[String, ?], String],
      cats.data.OptionT[Either[String, ?], Int](Right(Some(17))).ensure("error")(_ => true)
  )

}
