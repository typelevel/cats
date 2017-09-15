package cats.data

import cats.{Applicative, Eq}
import cats.kernel.instances.vector._

class ZipVector[A](val value: Vector[A]) extends AnyVal

object ZipVector {
  implicit val catsDataApplicativeForZipVector: Applicative[ZipVector] = new Applicative[ZipVector] {
    def pure[A](x: A): ZipVector[A] = new ZipVector(Vector(x))
    def ap[A, B](ff: ZipVector[A => B])(fa: ZipVector[A]): ZipVector[B] =
      new ZipVector((ff.value, fa.value).zipped.map(_ apply _))

    override def product[A, B](fa: ZipVector[A], fb: ZipVector[B]): ZipVector[(A, B)] =
      new ZipVector(fa.value.zip(fb.value))
  }

  implicit def catsDataEqForZipVector[A: Eq]: Eq[ZipVector[A]] = Eq.by(_.value)
}
