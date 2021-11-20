package cats
package data

class ZipLazyList[A](val value: LazyList[A]) extends AnyVal

object ZipLazyList {

  def apply[A](value: LazyList[A]): ZipLazyList[A] = new ZipLazyList(value)

  implicit val catsDataAlternativeForZipLazyList
    : Alternative[ZipLazyList] with CommutativeApplicative[ZipLazyList] with Selective[ZipLazyList] =
    new Alternative[ZipLazyList] with CommutativeApplicative[ZipLazyList] with Selective[ZipLazyList] {
      def pure[A](x: A): ZipLazyList[A] = new ZipLazyList(LazyList.continually(x))

      override def map[A, B](fa: ZipLazyList[A])(f: (A) => B): ZipLazyList[B] =
        ZipLazyList(fa.value.map(f))

      def ap[A, B](ff: ZipLazyList[A => B])(fa: ZipLazyList[A]): ZipLazyList[B] =
        ZipLazyList(ff.value.lazyZip(fa.value).map(_.apply(_)))

      override def product[A, B](fa: ZipLazyList[A], fb: ZipLazyList[B]): ZipLazyList[(A, B)] =
        ZipLazyList(fa.value.zip(fb.value))

      def select[A, B](fab: ZipLazyList[Either[A, B]])(ff: => ZipLazyList[A => B]): ZipLazyList[B] =
        ZipLazyList(fab.value.lazyZip(ff.value).map {
          case (Left(a), f)  => f(a)
          case (Right(b), _) => b
        })

      def empty[A]: ZipLazyList[A] = ZipLazyList(LazyList.empty[A])

      def combineK[A](x: ZipLazyList[A], y: ZipLazyList[A]): ZipLazyList[A] =
        ZipLazyList(cats.instances.lazyList.catsStdInstancesForLazyList.combineK(x.value, y.value))
    }

  implicit def catsDataEqForZipLazyList[A: Eq]: Eq[ZipLazyList[A]] =
    Eq.by((_: ZipLazyList[A]).value)(cats.kernel.instances.lazyList.catsKernelStdEqForLazyList[A])
}
