package cats

import cats.data.ZipStream

private[cats] trait ScalaVersionSpecificTraverseInstances {
  implicit def catsTraverseForStream: Traverse[Stream] = cats.instances.stream.catsStdInstancesForStream
}

private[cats] trait ScalaVersionSpecificShowInstances {
  implicit def catsShowForStream[A: Show]: Show[Stream[A]] = cats.instances.stream.catsStdShowForStream[A]
}

private[cats] trait ScalaVersionSpecificSemigroupalInstances {
  implicit def catsSemigroupalForStream: Semigroupal[Stream] = cats.instances.stream.catsStdInstancesForStream
}

private[cats] trait ScalaVersionSpecificMonoidKInstances {
  implicit def catsMonoidKForStream: MonoidK[Stream] = cats.instances.stream.catsStdInstancesForStream
}

private[cats] trait ScalaVersionSpecificParallelInstances {
  implicit def catsStdParallelForZipStream: Parallel.Aux[Stream, ZipStream] =
    cats.instances.stream.catsStdParallelForStreamZipStream
}

private[cats] trait ScalaVersionSpecificInvariantInstances {
  implicit def catsInstancesForStream: Monad[Stream] with Alternative[Stream] with CoflatMap[Stream] =
    cats.instances.stream.catsStdInstancesForStream
}

private[cats] trait ScalaVersionSpecificTraverseFilterInstances {
  implicit def catsTraverseFilterForStream: TraverseFilter[Stream] =
    cats.instances.stream.catsStdTraverseFilterForStream
}

private[cats] trait ScalaVersionSpecificAlignInstances
