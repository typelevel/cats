package cats
package instances

import scala.annotation.tailrec

trait MapInstances extends cats.kernel.instances.MapInstances {

  implicit def catsStdShowForMap[A, B](implicit showA: Show[A], showB: Show[B]): Show[Map[A, B]] =
    new Show[Map[A, B]] {
      def show(m: Map[A, B]): String =
        m.iterator
          .map { case (a, b) => showA.show(a) + " -> " + showB.show(b) }
          .mkString("Map(", ", ", ")")
    }

  // scalastyle:off method.length
  implicit def catsStdInstancesForMap[K]: Traverse[Map[K, ?]] with FlatMap[Map[K, ?]] =
    new Traverse[Map[K, ?]] with FlatMap[Map[K, ?]] {

      def traverse[G[_], A, B](fa: Map[K, A])(f: A => G[B])(implicit G: Applicative[G]): G[Map[K, B]] = {
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

      def foldLeft[A, B](fa: Map[K, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b) { case (x, (k, a)) => f(x, a)}

      def foldRight[A, B](fa: Map[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        Foldable.iterateRight(fa.values, lb)(f)

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

      override def size[A](fa: Map[K, A]): Long = fa.size.toLong

      override def get[A](fa: Map[K, A])(idx: Long): Option[A] =
        if (idx < 0L || Int.MaxValue < idx) None
        else {
          val n = idx.toInt
          if (n >= fa.size) None
          else Some(fa.valuesIterator.drop(n).next)
        }

      override def isEmpty[A](fa: Map[K, A]): Boolean = fa.isEmpty

      override def fold[A](fa: Map[K, A])(implicit A: Monoid[A]): A =
        A.combineAll(fa.values)

      override def toList[A](fa: Map[K, A]): List[A] = fa.values.toList
    }
  // scalastyle:on method.length
}
