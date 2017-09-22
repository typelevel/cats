package cats.data

import cats.{Alternative, Eq}
import cats.instances.vector._

class ZipVector[A](val value: Vector[A]) extends AnyVal

object ZipVector {

  def apply[A](value: Vector[A]): ZipVector[A] = new ZipVector(value)

  implicit val catsDataAlternativeForZipVector: Alternative[ZipVector] = new Alternative[ZipVector] {
    def pure[A](x: A): ZipVector[A] = new ZipVector(Vector(x))
    def ap[A, B](ff: ZipVector[A => B])(fa: ZipVector[A]): ZipVector[B] =
      ZipVector((ff.value, fa.value).zipped.map(_ apply _))

    override def product[A, B](fa: ZipVector[A], fb: ZipVector[B]): ZipVector[(A, B)] =
      ZipVector(fa.value.zip(fb.value))

    def empty[A]: ZipVector[A] = ZipVector(Vector.empty[A])

    def combineK[A](x: ZipVector[A], y: ZipVector[A]): ZipVector[A] =
      ZipVector(Alternative[Vector].combineK(x.value, y.value))
  }

  implicit def catsDataEqForZipVector[A: Eq]: Eq[ZipVector[A]] = Eq.by(_.value)
}
