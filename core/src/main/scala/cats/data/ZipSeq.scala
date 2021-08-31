package cats
package data

import scala.collection.immutable.Seq
import kernel.compat.scalaVersionSpecific._

class ZipSeq[A](val value: Seq[A]) extends AnyVal

@suppressUnusedImportWarningForScalaVersionSpecific
object ZipSeq {

  def apply[A](value: Seq[A]): ZipSeq[A] = new ZipSeq(value)

  implicit val catsDataCommutativeApplyForZipSeq: CommutativeApply[ZipSeq] = new CommutativeApply[ZipSeq] {

    override def map[A, B](fa: ZipSeq[A])(f: (A) => B): ZipSeq[B] =
      ZipSeq(fa.value.map(f))
    def ap[A, B](ff: ZipSeq[A => B])(fa: ZipSeq[A]): ZipSeq[B] =
      ZipSeq(ff.value.lazyZip(fa.value).map(_.apply(_)))

  }

  implicit def catsDataEqForZipSeq[A: Eq]: Eq[ZipSeq[A]] = Eq.by(_.value)
}
