package cats.data

import cats.{CommutativeApply, Eq}
import cats.instances.vector._

class ZipVector[A](val value: Vector[A]) extends AnyVal

object ZipVector {

  def apply[A](value: Vector[A]): ZipVector[A] = new ZipVector(value)

  implicit val catsDataCommutativeApplyForZipVector: CommutativeApply[ZipVector] = new CommutativeApply[ZipVector] {

    override def map[A, B](fa: ZipVector[A])(f: (A) => B): ZipVector[B] =
      ZipVector(fa.value.map(f))
    def ap[A, B](ff: ZipVector[A => B])(fa: ZipVector[A]): ZipVector[B] =
      ZipVector((ff.value, fa.value).zipped.map(_.apply(_)))

  }

  implicit def catsDataEqForZipVector[A: Eq]: Eq[ZipVector[A]] = Eq.by(_.value)
}
