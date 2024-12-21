/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package catsBC
import cats.InjectK
import cats.implicits.*

object MimaExceptions {

  def headOption[A](list: List[A]): Option[A] = list.headOption

  import cats.arrow.FunctionK // needs to be imported because of a hygiene problem

  def isBinaryCompatible = (
    cats.Monad[cats.data.OptionT[List, *]],
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
    cats.data.OptionT.catsDataMonadErrorForOptionT[Either[String, *], String],
    cats.data.OptionT[Either[String, *], Int](Right(Some(17))).ensure("error")(_ => true),
    "blah".leftNec[Int],
    List(Some(4), None).nested,
    cats.data.EitherT.left[Int](Option("err")),
    true.iterateUntilM(Option(_))(identity _),
    Either.catchOnly[NumberFormatException] { "foo".toInt },
    (1.validNel[String], 2.validNel[String], 3.validNel[String]).mapN(_ + _ + _),
    (1.asRight[String], 2.asRight[String], 3.asRight[String]).parMapN(_ + _ + _),
    InjectK.catsReflexiveInjectKInstance[Option],
    (
      cats.Bimonad[cats.data.NonEmptyChain],
      cats.NonEmptyTraverse[cats.data.NonEmptyChain],
      cats.SemigroupK[cats.data.NonEmptyChain]
    )
  )
}
