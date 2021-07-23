package cats
package free

/**
 * Free may be viewed either as a sort of sequential program construction mechanism, wherein
 * programs are assembled and then finally interpreted via foldMap, or it may be viewed as a
 * recursive data structure shaped by the ''pattern'' of its suspension functor. In this view,
 * it is helpful to think of Free as being the following recursive type alias:
 *
 * {{{
 * type Free[S[_], A] = Either[S[Free[S, A]], A]
 * }}}
 *
 * Thus, a Free is a tree of Either(s) in which the data (of type A) is at the leaves, and each
 * branching point is defined by the shape of the S functor. This kind of structural interpretation
 * of Free is very useful in certain contexts, such as when representing expression trees in
 * compilers and interpreters.
 *
 * Using this interpretation, we can define many common instances over the ''data structure'' of
 * Free, most notably Eq and Traverse (Show is also possible, though far less useful). This makes
 * it much more convenient to use Free structures as if they were conventional data structures.
 *
 * Unfortunately, this functionality fundamentally requires recursive implicit resolution. This
 * feature was added to Scala in 2.13 (and retained in Scala 3) in the form of by-name implicits,
 * but is fundamentally unavailable in Scala 2.12 and earlier without using something like Shapeless.
 * For that reason, all of these instances are only available on 2.13 and above.
 */
private trait FreeStructuralInstances {
  implicit def catsFreeEqForFree[S[_]: Functor, A: Eq](implicit S: => Eq[S[Free[S, A]]]): Eq[Free[S, A]] =
    Eq.instance { (left, right) =>
      (left.resume, right.resume) match {
        case (Right(leftA), Right(rightA)) =>
          Eq[A].eqv(leftA, rightA)

        case (Left(leftS), Left(rightS)) =>
          S.eqv(leftS, rightS)

        case (Left(_), Right(_)) | (Right(_), Left(_)) =>
          false
      }
    }
}
