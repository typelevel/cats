package fix
package to1_0_0

object RemoveUnapplyTests {
  import cats.implicits._
  import cats.Foldable
  def parseInt(s: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](s.toInt).leftMap(_ => "no number")
  val ns = List("1", "2", "3")
  ns.traverse(parseInt)
  ns.traverse_(parseInt)
  Foldable[List].traverse_(ns)(parseInt)

  import cats.data.{Validated, ValidatedNel}
  val x: List[ValidatedNel[String, Int]] =
    List(Validated.valid(1), Validated.invalid("a"), Validated.invalid("b"))
      .map(_.toValidatedNel)
  x.sequence
  x.sequence_
  Foldable[List].sequence_(x)

  import cats.data.Func.appFunc
  import cats.data.State.{get, set}
  import cats.data.Const
  type Count[A] = Const[Int, A]
  def liftInt(i: Int): Count[Unit] = Const(i)
  def isSpace(c: Char): Boolean = (c == ' ' || c == '\n')
  def testIf(b: Boolean): Int = if (b) 1 else 0
  appFunc { (c: Char) =>
    for {
      x <- get[Boolean]
      y = !isSpace(c)
      _ <- set(y)
    } yield testIf(y && !x)
  } andThen appFunc(liftInt)

  import cats.free.FreeT
  val a: Either[String, Int] = Right(42)
  FreeT.liftT(a)

}
