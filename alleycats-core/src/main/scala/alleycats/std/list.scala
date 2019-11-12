package alleycats
package std

object list extends ListInstances

trait ListInstances {
  implicit val alleycatsStdListEmptyK: EmptyK[List] =
    new EmptyK[List] {
      def empty[A]: List[A] = Nil
    }

  implicit val alleycatsStdListConsK: ConsK[List] =
    new ConsK[List] {
      def cons[A](hd: A, tl: List[A]): List[A] = hd :: tl
    }
}
