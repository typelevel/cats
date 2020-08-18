package cats.tests

import cats.{InvariantMonoidal, InvariantSemigroupal, InvariantSemiringal}
import cats.kernel.{Eq, Monoid, Semigroup}
import cats.kernel.compat.scalaVersionSpecific._
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import cats.laws.discipline.{ExhaustiveCheck, InvariantSemiringalTests, MiniInt, SerializableTests}
import cats.syntax.all._
import org.scalacheck.{Arbitrary, Gen}

@suppressUnusedImportWarningForScalaVersionSpecific
object BinCodecInvariantMonoidalSuite {
  final case class MiniList[+A] private (val toList: List[A]) extends AnyVal {
    import MiniList.truncated

    /**
     * Returns a new MiniList with a modified underlying List. Note that this truncates the returned List to ensure that
     * it fits into the allowable MiniInt range.
     */
    def mod[B](f: List[A] => List[B]): MiniList[B] = truncated(f(toList))

    /**
     * Append a `MiniList`.
     *
     * Note: this will trim the resulting list to respect the maximum list length.
     */
    def ++[AA >: A](o: MiniList[AA]): MiniList[AA] = mod(_ ++ o.toList)
  }

  object MiniList {
    val maxLength: Int = 6

    val nil: MiniList[Nothing] = MiniList(Nil)
    def empty[A]: MiniList[A] = nil

    def truncated[A](l: List[A]): MiniList[A] = MiniList(l.take(maxLength))

    def unsafe[A](l: List[A]): MiniList[A] = {
      val inputLength = l.size
      if (inputLength > maxLength)
        throw new IllegalArgumentException(
          s"MiniList.unsafe called with list of size $inputLength, but $maxLength is the maximum allowed size."
        )
      else MiniList(l)
    }

    def one[A](a: A): MiniList[A] = MiniList(a :: Nil)

    implicit def eqForMiniList[A: Eq]: Eq[MiniList[A]] = Eq.by(_.toList)

    implicit val exhaustiveCheckForMiniListBoolean: ExhaustiveCheck[MiniList[Boolean]] =
      ExhaustiveCheck.instance(
        for {
          length <- (0 to maxLength).toList
          boolList <- List(false, true).replicateA(length)
        } yield MiniList.unsafe(boolList)
      )
  }

  /**
   * A small amount of binary bits
   */
  type Bin = MiniList[Boolean]

  /**
   * Type class to read and write objects of type A to binary.
   *
   * Obeys `forAll { (c: BinCodec[A], a: A) => c.read(c.writes(a)) == (Some(a), List())`,
   * under the assumtion that `imap(f, g)` is always called with `f` and `g` such that
   * `forAll { (a: A) => g(f(a)) == a }`.
   */
  trait BinCodec[A] extends Serializable { self =>

    /**
     * Reads the first value of a Bin, returning an optional value of type `A` and the remaining Bin.
     */
    def read(s: Bin): (Option[A], Bin)

    /**
     * Writes a value of type `A` to Bin format.
     */
    def write(a: A): Bin

    def supported: Set[A]
  }

  object BinCodec {
    // In tut/invariantmonoidal.md pure, product and imap are defined in
    // their own trait to be introduced one by one,
    trait CCPure {
      def unit: BinCodec[Unit] =
        new BinCodec[Unit] {
          def read(s: Bin): (Option[Unit], Bin) = (Some(()), s)
          def write(a: Unit): Bin = MiniList.empty

          def supported: Set[Unit] = Set(())
        }
    }

    trait CCProduct {
      def product[A, B](fa: BinCodec[A], fb: BinCodec[B]): BinCodec[(A, B)] =
        new BinCodec[(A, B)] {
          def read(s: Bin): (Option[(A, B)], Bin) = {
            val (a1, s1) = fa.read(s)
            val (a2, s2) = fb.read(s1)
            ((a1, a2).mapN(_ -> _), s2)
          }

          def write(a: (A, B)): Bin =
            fa.write(a._1) ++ fb.write(a._2)

          def supported: Set[(A, B)] = fa.supported.flatMap(a => fb.supported.map(b => (a, b)))
        }
    }

    trait CCImap {
      def imap[A, B](fa: BinCodec[A])(f: A => B)(g: B => A): BinCodec[B] =
        new BinCodec[B] {
          def read(s: Bin): (Option[B], Bin) = {
            val (a1, s1) = fa.read(s)
            (a1.map(f), s1)
          }

          def write(a: B): Bin =
            fa.write(g(a))

          def supported: Set[B] = fa.supported.map(f)
        }
    }

    trait CCZero {
      def zero: BinCodec[Nothing] =
        new BinCodec[Nothing] {
          def read(s: Bin): (Option[Nothing], Bin) = (None, s)

          def write(a: Nothing): Bin = MiniList.nil

          def supported: Set[Nothing] = Set.empty
        }
    }

    trait CCChoice {
      def choice[A, B](fa: BinCodec[A], fb: BinCodec[B]) =
        new BinCodec[Either[A, B]] {
          def read(s: Bin): (Option[Either[A, B]], Bin) = {
            val (oa, bin) = fa.read(s)
            oa match {
              case Some(a) => (Some(Left(a)), bin)
              case None =>
                val (ob, bin2) = fb.read(s)
                (ob.map(Right(_)), bin2)
            }

          }
          def write(a: Either[A, B]): Bin =
            a match {
              case Left(a)  => fa.write(a)
              case Right(b) => fb.write(b)
            }

          def supported: Set[Either[A, B]] = fa.supported.map(Left(_): Either[A, B]).union(fb.supported.map(Right(_)))
        }
    }

    implicit val binCodecIsInvariantSemiringal: InvariantSemiringal[BinCodec] =
      new InvariantSemiringal[BinCodec] with CCPure with CCProduct with CCImap with CCChoice with CCZero
  }

  def genBinCodecForExhaustive[A](implicit exA: ExhaustiveCheck[A]): Gen[BinCodec[A]] =
    for {
      bitCount <- Gen.oneOf(1, 2, 3)
      shuffleSeed <- Gen.choose(Long.MinValue, Long.MaxValue)
    } yield {
      val binValues: List[Bin] = List(false, true).replicateA(bitCount).map(MiniList.unsafe(_))
      val pairs: List[(A, Bin)] = new scala.util.Random(seed = shuffleSeed).shuffle(exA.allValues).toList.zip(binValues)
      val aToBin: Map[A, Bin] = pairs.toMap
      val binToA: Map[Bin, A] = pairs.map(_.swap).toMap

      new BinCodec[A] {
        def read(s: Bin): (Option[A], Bin) =
          (binToA.get(s.mod(_.take(bitCount))), s.mod(_.drop(bitCount)))

        def write(a: A): Bin =
          aToBin.getOrElse(a, MiniList.empty)

        override def toString: String = s"BinCodec($pairs)"

        def supported: Set[A] = aToBin.keySet
      }
    }

  implicit val arbMiniIntCodec: Arbitrary[BinCodec[MiniInt]] =
    Arbitrary(genBinCodecForExhaustive[MiniInt])

  implicit val arbBooleanCodec: Arbitrary[BinCodec[Boolean]] =
    Arbitrary(genBinCodecForExhaustive[Boolean])

  implicit def binCodecsEq[A: Eq]: Eq[BinCodec[A]] = { (x, y) =>
    val xBins: Set[Bin] = x.supported.map(x.write)
    val yBins: Set[Bin] = y.supported.map(y.write)

    val xxRead = xBins.map(x.read)
    val xyRead = xBins.map(y.read)
    val yyRead = yBins.map(y.read)
    val yxRead = yBins.map(x.read)

    xBins === yBins && xxRead === xyRead && xyRead === yyRead && yyRead === yxRead
  }
}

class BinCodecInvariantMonoidalSuite extends CatsSuite {
  // Everything is defined in a companion object to be serializable.
  import BinCodecInvariantMonoidalSuite._

  implicit val eqBinCodecNothing: Eq[Nothing] = Eq.allEqual

  checkAll("InvariantSemiringal[BinCodec]",
           InvariantSemiringalTests[BinCodec].invariantSemiringal[Boolean, Boolean, Boolean](Eq[BinCodec[Nothing]](binCodecsEq[Nothing]))
  )
  checkAll("InvariantSemiringal[BinCodec]", SerializableTests.serializable(InvariantSemiringal[BinCodec]))

  {
    implicit val miniIntMonoid: Monoid[MiniInt] = MiniInt.miniIntAddition
    implicit val binMonoid: Monoid[BinCodec[MiniInt]] = InvariantMonoidal.monoid[BinCodec, MiniInt]
    checkAll("InvariantMonoidal[BinCodec].monoid", MonoidTests[BinCodec[MiniInt]].monoid)
  }

  {
    implicit val miniIntSemigroup: Semigroup[MiniInt] = MiniInt.miniIntAddition
    implicit val binSemigroup: Semigroup[BinCodec[MiniInt]] = InvariantSemigroupal.semigroup[BinCodec, MiniInt]
    checkAll("InvariantSemigroupal[BinCodec].semigroup", SemigroupTests[BinCodec[MiniInt]].semigroup)
  }
}
