package cats.instances

import cats.kernel.compat.scalaVersionSpecific._
import scala.collection.mutable.Builder

@suppressUnusedImportWarningForScalaVersionSpecific
private[instances] object instances {

  def appendAll[F <: Iterable[A], A](as: IterableOnce[F], bldr: Builder[A, F]): bldr.type = {
    val it = as.iterator
    while (it.hasNext) {
      bldr ++= it.next()
    }
    bldr
  }

}
