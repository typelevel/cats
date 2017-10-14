package cats.data

import cats.{Apply, Eq, Semigroup}
import cats.instances.map._

class ZipMap[K, A](val value: Map[K, A]) extends AnyVal

object ZipMap {

  def apply[K, A](value: Map[K, A]): ZipMap[K, A] = new ZipMap(value)

  implicit def catsDataApplyForZipMap[K: Semigroup]: Apply[ZipMap[K, ?]] = new Apply[ZipMap[K, ?]] {

    override def map[A, B](fa: ZipMap[K, A])(f: (A) => B): ZipMap[K, B] =
      ZipMap(fa.value.map { case (k, a) => (k, f(a)) })
    def ap[A, B](ff: ZipMap[K, A => B])(fa: ZipMap[K, A]): ZipMap[K, B] =
      ZipMap((ff.value, fa.value).zipped.map { case ((k1, f), (k2, a)) => (Semigroup[K].combine(k1, k2), f(a)) })

  }

  implicit def catsDataEqForZipMap[K: Eq, A: Eq]: Eq[ZipMap[K, A]] = Eq.by(_.value)
}