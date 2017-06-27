package cats
package tests

import cats.laws.discipline.eq.catsLawsEqForFn1
import cats.laws.discipline.{InvariantMonoidalTests, SerializableTests}
import cats.instances.all._
import cats.syntax.apply._
import cats.Eq
import org.scalacheck.{Arbitrary, Gen}

object CsvCodecInvariantMonoidalTests {
  type CSV = List[String]

  /**
   * Type class to read and write objects of type A to CSV.
   *
   * Obeys `forAll { (c: CsvCodec[A], a: A) => c.read(c.writes(a)) == (Some(a), List())`,
   * under the assumtion that `imap(f, g)` is always called with `f` and `g` such that
   * `forAll { (a: A) => g(f(a)) == a }`.
   */
  trait CsvCodec[A] extends Serializable { self =>
    /** Reads the first value of a CSV, returning an optional value of type `A` and the remaining CSV. */
    def read(s: CSV): (Option[A], CSV)

    /** Writes a value of type `A` to CSV format. */
    def write(a: A): CSV
  }

  object CsvCodec {
    // In tut/invariantmonoidal.md pure, product and imap are defined in
    // their own trait to be introduced one by one,
    trait CCPure {
      def pure[A](a: A): CsvCodec[A] = new CsvCodec[A] {
        def read(s: CSV): (Option[A], CSV) = (Some(a), s)
        def write(a: A): CSV = List.empty
      }
    }

    trait CCProduct {
      def product[A, B](fa: CsvCodec[A], fb: CsvCodec[B]): CsvCodec[(A, B)] =
        new CsvCodec[(A, B)] {
          def read(s: CSV): (Option[(A, B)], CSV) = {
            val (a1, s1) = fa.read(s)
            val (a2, s2) = fb.read(s1)
            ((a1,  a2).mapN(_ -> _), s2)
          }

          def write(a: (A, B)): CSV =
            fa.write(a._1) ++ fb.write(a._2)
        }
    }

    trait CCImap {
      def imap[A, B](fa: CsvCodec[A])(f: A => B)(g: B => A): CsvCodec[B] =
        new CsvCodec[B] {
          def read(s: CSV): (Option[B], CSV) = {
            val (a1, s1) = fa.read(s)
            (a1.map(f), s1)
          }

          def write(a: B): CSV =
            fa.write(g(a))
        }
    }

    implicit val csvCodecIsInvariantMonoidal: InvariantMonoidal[CsvCodec] =
      new InvariantMonoidal[CsvCodec] with CCPure with CCProduct with CCImap
  }

  def numericSystemCodec(base: Int): CsvCodec[Int] =
    new CsvCodec[Int] {
      def read(s: CSV): (Option[Int], CSV) =
        (s.headOption.flatMap(head => scala.util.Try(Integer.parseInt(head, base)).toOption), s.drop(1))

      def write(a: Int): CSV =
        List(Integer.toString(a, base))
    }

  implicit val arbNumericSystemCodec: Arbitrary[CsvCodec[Int]] =
    Arbitrary(Gen.choose(2, 16).map(numericSystemCodec))

  implicit def csvCodecsEq[A](implicit a: Arbitrary[A], e: Eq[A]): Eq[CsvCodec[A]] =
    catsLawsEqForFn1[A, CSV].on[CsvCodec[A]](_.write) and catsLawsEqForFn1[CSV, (Option[A], CSV)].on[CsvCodec[A]](_.read)
}

class CsvCodecInvariantMonoidalTests extends CatsSuite {
  // Eveything is defined in a companion object to be serializable.
  import CsvCodecInvariantMonoidalTests._

  checkAll("InvariantMonoidal[CsvCodec]", InvariantMonoidalTests[CsvCodec].invariantMonoidal[Int, Int, Int])
  checkAll("InvariantMonoidal[CsvCodec]", SerializableTests.serializable(InvariantMonoidal[CsvCodec]))
}
