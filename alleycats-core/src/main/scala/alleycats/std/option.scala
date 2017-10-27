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
}
