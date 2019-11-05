package cats
package instances

import scala.util.control.TailCalls.{done, tailcall, TailRec}

trait TailRecInstances {
  implicit def catsInstancesForTailRec: StackSafeMonad[TailRec] with Defer[TailRec] =
    TailRecInstances.catsInstancesForTailRec
}

private object TailRecInstances {
  val catsInstancesForTailRec: StackSafeMonad[TailRec] with Defer[TailRec] =
    new StackSafeMonad[TailRec] with Defer[TailRec] {
      def defer[A](fa: => TailRec[A]): TailRec[A] = tailcall(fa)

      def pure[A](a: A): TailRec[A] = done(a)

      override def map[A, B](fa: TailRec[A])(f: A => B): TailRec[B] =
        fa.map(f)

      def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
        fa.flatMap(f)

      override val unit: TailRec[Unit] = done(())
    }
}
