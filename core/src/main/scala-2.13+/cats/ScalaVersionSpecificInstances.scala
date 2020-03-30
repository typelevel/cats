package cats

import cats.data.{ZipLazyList, ZipStream}
import scala.collection.immutable.ArraySeq

private[cats] trait ScalaVersionSpecificTraverseInstances {
  @deprecated("Use catsTraverseForLazyList", "3.0.0")
  implicit def catsTraverseForStream: Traverse[Stream] = cats.instances.stream.catsStdInstancesForStream

  implicit def catsTraverseForLazyList: Traverse[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
  implicit def catsTraverseForArraySeq: Traverse[ArraySeq] = cats.instances.arraySeq.catsStdInstancesForArraySeq
}

private[cats] trait ScalaVersionSpecificShowInstances {
  @deprecated("Use catsShowForLazyList", "3.0.0")
  implicit def catsShowForStream[A: Show]: Show[Stream[A]] = cats.instances.stream.catsStdShowForStream[A]

  implicit def catsShowForLazyList[A: Show]: Show[LazyList[A]] = cats.instances.lazyList.catsStdShowForLazyList[A]
  implicit def catsShowForArraySeq[A: Show]: Show[ArraySeq[A]] = cats.instances.arraySeq.catsStdShowForArraySeq[A]
}

private[cats] trait ScalaVersionSpecificSemigroupalInstances {
  @deprecated("Use catsSemigroupalForLazyList", "3.0.0")
  implicit def catsSemigroupalForStream: Semigroupal[Stream] = cats.instances.stream.catsStdInstancesForStream

  implicit def catsSemigroupalForLazyList: Semigroupal[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
  implicit def catsSemigroupalForArraySeq: Semigroupal[ArraySeq] = cats.instances.arraySeq.catsStdInstancesForArraySeq
}

private[cats] trait ScalaVersionSpecificMonoidKInstances {
  @deprecated("Use catsMonoidKForLazyList", "3.0.0")
  implicit def catsMonoidKForStream: MonoidK[Stream] = cats.instances.stream.catsStdInstancesForStream

  implicit def catsMonoidKForLazyList: MonoidK[LazyList] = cats.instances.lazyList.catsStdInstancesForLazyList
  implicit def catsMonoidKForArraySeq: MonoidK[ArraySeq] = cats.instances.arraySeq.catsStdInstancesForArraySeq
}

private[cats] trait ScalaVersionSpecificParallelInstances {
  @deprecated("Use catsParallelForLazyList", "3.0.0")
  implicit def catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    cats.instances.parallel.catsStdParallelForZipStream

  implicit def catsStdParallelForZipLazyList: Parallel.Aux[LazyList, ZipLazyList] =
    cats.instances.lazyList.catsStdParallelForLazyListZipLazyList
}

private[cats] trait ScalaVersionSpecificInvariantInstances {
  @deprecated("Use catsInstancesForLazyList", "3.0.0")
  implicit def catsInstancesForStream: Monad[Stream] with Alternative[Stream] with CoflatMap[Stream] =
    cats.instances.stream.catsStdInstancesForStream

  implicit def catsInstancesForLazyList: Monad[LazyList] with Alternative[LazyList] with CoflatMap[LazyList] =
    cats.instances.lazyList.catsStdInstancesForLazyList

  implicit def catsInstancesForArraySeq: Monad[ArraySeq] with Alternative[ArraySeq] with CoflatMap[ArraySeq] =
    cats.instances.arraySeq.catsStdInstancesForArraySeq
}

private[cats] trait ScalaVersionSpecificTraverseFilterInstances {
  @deprecated("Use catsTraverseFilterForLazyList", "3.0.0")
  implicit def catsTraverseFilterForStream: TraverseFilter[Stream] =
    cats.instances.stream.catsStdTraverseFilterForStream

  implicit def catsTraverseFilterForLazyList: TraverseFilter[LazyList] =
    cats.instances.lazyList.catsStdTraverseFilterForLazyList

  implicit def catsTraverseFilterForArraySeq: TraverseFilter[ArraySeq] =
    cats.instances.arraySeq.catsStdTraverseFilterForArraySeq
}

private[cats] trait ScalaVersionSpecificAlignInstances {
  @deprecated("Use catsTraverseFilterForLazyList", "3.0.0")
  implicit def catsAlignForStream: Align[Stream] =
    cats.instances.stream.catsStdInstancesForStream

  implicit def catsAlignForLazyList: Align[LazyList] =
    cats.instances.lazyList.catsStdInstancesForLazyList

  implicit def catsAlignForArraySeq: Align[ArraySeq] =
    cats.instances.arraySeq.catsStdInstancesForArraySeq
}
