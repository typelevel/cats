package alleycats
package std

import export._

@reexports(ListInstances)
object list

@exports
object ListInstances {
  @export(Orphan)
  implicit val exportListEmptyK: EmptyK[List] =
    new EmptyK[List] {
      def empty[A]: List[A] = Nil
    }

  @export(Orphan)
  implicit val exportListConsK: ConsK[List] =
    new ConsK[List] {
      def cons[A](hd: A, tl: List[A]): List[A] = hd :: tl
    }
}
