package alleycats
package std

object option extends OptionInstances

trait OptionInstances {
  implicit val alleycatsStdOptionEmptyK: EmptyK[Option] =
    new EmptyK[Option] {
      def empty[A]: Option[A] = None
    }
}
