package cats
package instances

import cats.kernel.CommutativeMonoid

import scala.annotation.tailrec
import cats.arrow.Compose

trait MapInstances extends cats.kernel.instances.MapInstances {

  implicit def catsStdShowForMap[A, B](implicit showA: Show[A], showB: Show[B]): Show[Map[A, B]] =
    new Show[Map[A, B]] {
      def show(m: Map[A, B]): String =
        m.iterator
          .map { case (a, b) => showA.show(a) + " -> " + showB.show(b) }
          .mkString("Map(", ", ", ")")
    }

  // scalastyle:off method.length
  implicit def catsStdInstancesForMap[K]: UnorderedTraverse[Map[K, ?]] with FlatMap[Map[K, ?]] =
    new UnorderedTraverse[Map[K, ?]] with FlatMap[Map[K, ?]] {

      def unorderedTraverse[G[_], A, B](fa: Map[K, A])(f: A => G[B])(implicit G: CommutativeApplicative[G]): G[Map[K, B]] = {
        val gba: Eval[G[Map[K, B]]] = Always(G.pure(Map.empty))
        val gbb = Foldable.iterateRight(fa, gba){ (kv, lbuf) =>
          G.map2Eval(f(kv._2), lbuf)({ (b, buf) => buf + (kv._1 -> b)})
        }.value
        G.map(gbb)(_.toMap)
      }

      override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
        fa.map { case (k, a) => (k, f(a)) }

      override def map2[A, B, Z](fa: Map[K, A], fb: Map[K, B])(f: (A, B) => Z): Map[K, Z] =
        if (fb.isEmpty) Map.empty // do O(1) work if fb is empty
        else fa.flatMap { case (k, a) => fb.get(k).map(b => (k, f(a, b))) }

      override def map2Eval[A, B, Z](fa: Map[K, A], fb: Eval[Map[K, B]])(f: (A, B) => Z): Eval[Map[K, Z]] =
        if (fa.isEmpty) Eval.now(Map.empty) // no need to evaluate fb
        else fb.map(fb => map2(fa, fb)(f))

      override def ap[A, B](ff: Map[K, A => B])(fa: Map[K, A]): Map[K, B] =
        fa.flatMap { case (k, a) => ff.get(k).map(f => (k, f(a))) }

      override def ap2[A, B, Z](f: Map[K, (A, B) => Z])(fa: Map[K, A], fb: Map[K, B]): Map[K, Z] =
        f.flatMap { case (k, f) =>
          for { a <- fa.get(k); b <- fb.get(k) } yield (k, f(a, b))
        }

      def flatMap[A, B](fa: Map[K, A])(f: (A) => Map[K, B]): Map[K, B] =
        fa.flatMap { case (k, a) => f(a).get(k).map((k, _)) }

      def unorderedFoldMap[A, B: CommutativeMonoid](fa: Map[K, A])(f: (A) => B) =
        fa.foldLeft(Monoid[B].empty){ case (b, (k, a)) => Monoid[B].combine(b, f(a)) }

      def tailRecM[A, B](a: A)(f: A => Map[K, Either[A, B]]): Map[K, B] = {
        val bldr = Map.newBuilder[K, B]

        @tailrec def descend(k: K, either: Either[A, B]): Unit =
          either match {
            case Left(a) =>
              f(a).get(k) match {
                case Some(x) => descend(k, x)
                case None => ()
              }
            case Right(b) =>
              bldr += ((k, b))
              ()
          }

        f(a).foreach { case (k, a) => descend(k, a) }
        bldr.result
      }


      override def isEmpty[A](fa: Map[K, A]): Boolean = fa.isEmpty

      override def unorderedFold[A](fa: Map[K, A])(implicit A: CommutativeMonoid[A]): A =
        A.combineAll(fa.values)

    }
  // scalastyle:on method.length

}

trait MapInstancesBinCompat0 {

  implicit val catsStdComposeForMap: Compose[Map] = new Compose[Map] {

    /**
      * Compose two maps `g` and `f` by using the values in `f` as keys for `g`.
      * {{{
      * scala> import cats.arrow.Compose
      * scala> import cats.implicits._
      * scala> val first = Map(1 -> "a", 2 -> "b", 3 -> "c", 4 -> "a")
      * scala> val second = Map("a" -> true, "b" -> false, "d" -> true)
      * scala> Compose[Map].compose(second, first)
      * res0: Map[Int, Boolean] = Map(1 -> true, 2 -> false, 4 -> true)
      * }}}
      */
    def compose[A, B, C](f: Map[B, C], g: Map[A, B]): Map[A, C] = {
      g.foldLeft(Map.empty[A, C]) {
        case (acc, (key, value)) =>
          f.get(value) match {
            case Some(other) => acc + (key -> other)
            case _ => acc
          }
      }
    }

  }

}
