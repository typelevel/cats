package cats
package tests

import cats.instances.AllInstances
import cats.syntax.AllSyntax

/**
 * Test that MTL usage works. Successful compilation is all
 * that's needed here.
 */
object MTLTests extends AllInstances with AllSyntax {
  def app[F[_], E, R, S, W](implicit
    F0: Monad[F],
    F1: MonadFilter[F],
    F2: TraverseFilter[F],
    F3: MonadError[F, E],
    F4: MonadReader[F, R],
    F5: MonadState[F, S],
    F6: MonadWriter[F, W]
  ): F[Unit] = {
    val x = F4.ask

    /** monads work */
    val a = for {
      _ <- x
      _ <- x
    } yield 42

    /** traverse filter */
    val b = a.traverseFilter(_ => List.empty[Option[Double]])

    ().pure[F]
  }
}
