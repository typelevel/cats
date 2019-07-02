package cats.kernel.compat
import scala.annotation.{Annotation, StaticAnnotation}
import scala.collection.{IterableLike, TraversableLike}

private[cats] object scalaVersionSpecific {

  /**
   * a trick to suppress unused import warning for this object
   */
  class suppressUnusedImportWarningForScalaVersionSpecific extends Annotation with StaticAnnotation

  type LazyList[+A] = Stream[A]
  val LazyList = Stream
  type IterableOnce[+A] = TraversableOnce[A]

  implicit class traversableOnceExtension[A](private val to: TraversableOnce[A]) extends AnyVal {
    def iterator: Iterator[A] = to.toIterator
  }

  implicit class doubleExtension(private val double: Double) extends AnyVal {
    def sign: Double = if (double.isNaN) Double.NaN else double.signum.toDouble
  }
  implicit class intExtension(private val i: Int) extends AnyVal {
    def sign: Int = i.signum
  }

  implicit class lazyZipExtension[A](private val a: A) extends AnyVal {
    def lazyZip[El1, Repr1, El2, Repr2, T](that: T)(implicit w1: A => TraversableLike[El1, Repr1],
                                                    w2: T => IterableLike[El2, Repr2]) = (a, that).zipped
  }
}
