package cats
package tests

import cats.laws.discipline.eq._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.{ExhaustiveCheck, InvariantMonoidalTests, MiniInt, SerializableTests}
import cats.implicits._
import cats.Eq
import cats.kernel.laws.discipline.{MonoidTests, SemigroupTests}
import org.scalacheck.{Arbitrary, Gen}

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
          length <- (0 to maxLength).toStream
          boolList <- List(false, true).replicateA(length).toStream
        } yield MiniList.unsafe(boolList)
      )
  }

  /** A small amount of binary bits */
  type Bin = MiniList[Boolean]

  /**
   * Type class to read and write objects of type A to binary.
   *
   * Obeys `forAll { (c: BinCodec[A], a: A) => c.read(c.writes(a)) == (Some(a), List())`,
   * under the assumtion that `imap(f, g)` is always called with `f` and `g` such that
   * `forAll { (a: A) => g(f(a)) == a }`.
   */
  trait BinCodec[A] extends Serializable { self =>

    /** Reads the first value of a Bin, returning an optional value of type `A` and the remaining Bin. */
    def read(s: Bin): (Option[A], Bin)

    /** Writes a value of type `A` to Bin format. */
    def write(a: A): Bin
  }

  object BinCodec {
    // In tut/invariantmonoidal.md pure, product and imap are defined in
    // their own trait to be introduced one by one,
    trait CCPure {
      def unit: BinCodec[Unit] = new BinCodec[Unit] {
        def read(s: Bin): (Option[Unit], Bin) = (Some(()), s)
        def write(a: Unit): Bin = MiniList.empty
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
        }
    }

    implicit val binCodecIsInvariantMonoidal: InvariantMonoidal[BinCodec] =
      new InvariantMonoidal[BinCodec] with CCPure with CCProduct with CCImap
  }

  def genBinCodecForExhaustive[A](implicit exA: ExhaustiveCheck[A]): Gen[BinCodec[A]] =
    for {
      bitCount <- Gen.oneOf(1, 2, 3)
      shuffleSeed <- Gen.choose(Long.MinValue, Long.MaxValue)
    } yield {
      val binValues: Stream[Bin] = Stream(false, true).replicateA(bitCount).map(MiniList.unsafe(_))
      val pairs: List[(A, Bin)] = new scala.util.Random(seed = shuffleSeed).shuffle(exA.allValues).toList.zip(binValues)
      val aToBin: Map[A, Bin] = pairs.toMap
      val binToA: Map[Bin, A] = pairs.map(_.swap).toMap

      new BinCodec[A] {
        def read(s: Bin): (Option[A], Bin) =
          (binToA.get(s.mod(_.take(bitCount))), s.mod(_.drop(bitCount)))

        def write(a: A): Bin =
          aToBin.getOrElse(a, MiniList.empty)

        override def toString: String = s"BinCodec($pairs)"
      }
    }

  implicit val arbMiniIntCodec: Arbitrary[BinCodec[MiniInt]] =
    Arbitrary(genBinCodecForExhaustive[MiniInt])

  implicit val arbBooleanCodec: Arbitrary[BinCodec[Boolean]] =
    Arbitrary(genBinCodecForExhaustive[Boolean])

  implicit def binCodecsEq[A: Eq: ExhaustiveCheck]: Eq[BinCodec[A]] = {
    val writeEq: Eq[BinCodec[A]] = Eq.by[BinCodec[A], A => Bin](_.write)

    val readEq: Eq[BinCodec[A]] = Eq.by[BinCodec[A], Bin => (Option[A], Bin)](_.read)
    Eq.and(writeEq, readEq)
  }
}

class BinCodecInvariantMonoidalSuite extends CatsSuite {
  // Eveything is defined in a companion object to be serializable.
  import BinCodecInvariantMonoidalSuite._

  checkAll("InvariantMonoidal[BinCodec]", InvariantMonoidalTests[BinCodec].invariantMonoidal[MiniInt, MiniInt, MiniInt])
  checkAll("InvariantMonoidal[BinCodec]", SerializableTests.serializable(InvariantMonoidal[BinCodec]))

  {
    implicit val miniIntMonoid: Monoid[MiniInt] = MiniInt.miniIntAddition
    implicit val binMonoid = InvariantMonoidal.monoid[BinCodec, MiniInt]
    checkAll("InvariantMonoidal[BinCodec].monoid", MonoidTests[BinCodec[MiniInt]].monoid)
  }

  {
    implicit val miniIntSemigroup: Semigroup[MiniInt] = MiniInt.miniIntAddition
    implicit val binSemigroup = InvariantSemigroupal.semigroup[BinCodec, MiniInt]
    checkAll("InvariantSemigroupal[BinCodec].semigroup", SemigroupTests[BinCodec[MiniInt]].semigroup)
  }
}
