package cats.compat

import scala.collection.immutable.{Seq => ISeq}

private[cats] object Seq {
  def zipWith[A, B, C](fa: ISeq[A], fb: ISeq[B])(f: (A, B) => C): ISeq[C] =
    fa.lazyZip(fb).map(f)
}
