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
    cats.data.OptionT[Either[String, ?], Int](Right(Some(17))).ensure("error")(_ => true),
    "blah".leftNec[Int],
    List(Some(4), None).nested,
    cats.data.EitherT.left[Int](Option("err")),
    true.iterateUntilM(Option(_))(identity _),
    Either.catchOnly[NumberFormatException] { "foo".toInt },
    (1.validNel[String], 2.validNel[String], 3.validNel[String]) mapN (_ + _ + _),
    (1.asRight[String], 2.asRight[String], 3.asRight[String]) parMapN (_ + _ + _)
  )
}
