package alleycats
package std

import export._

@reexports(OptionInstances)
object option

@exports
object OptionInstances {
  @export(Orphan)
  implicit val exportOptionEmptyK: EmptyK[Option] =
    new EmptyK[Option] {
      def empty[A]: Option[A] = None
    }

  @export(Orphan)
  implicit val exportOptionOrElse: OrElse[Option] =
    new OrElse[Option] {
      override def orElse[A](fa: Option[A], alternative: => Option[A]): Option[A] =
        fa.orElse(alternative)
    }
}
