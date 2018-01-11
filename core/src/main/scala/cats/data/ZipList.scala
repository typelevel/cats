package cats.data

import cats.{CommutativeApply, Eq}
import cats.instances.list.catsKernelStdEqForList

class ZipList[A](val value: List[A]) extends AnyVal

object ZipList {

  def apply[A](value: List[A]): ZipList[A] = new ZipList(value)

  implicit val catsDataCommutativeApplyForZipList: CommutativeApply[ZipList] = new CommutativeApply[ZipList] {

    override def map[A, B](fa: ZipList[A])(f: (A) => B): ZipList[B] =
      ZipList(fa.value.map(f))

    def ap[A, B](ff: ZipList[A => B])(fa: ZipList[A]): ZipList[B] =
      ZipList((ff.value, fa.value).zipped.map(_ apply _))

    override def product[A, B](fa: ZipList[A], fb: ZipList[B]): ZipList[(A, B)] =
      ZipList(fa.value.zip(fb.value))

  }

  implicit def catsDataEqForZipList[A: Eq]: Eq[ZipList[A]] = Eq.by(_.value)
}
