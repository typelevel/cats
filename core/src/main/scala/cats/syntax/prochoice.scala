package cats.syntax

import cats.functor.ProChoice

trait ProChoiceSyntax {
  // TODO: use simulacrum instances eventually
  implicit def proChoiceSyntax[F[_, _]: ProChoice, A, B](fab: F[A, B]): ProChoiceOps[F, A, B] =
    new ProChoiceOps[F, A, B](fab)
}

class ProChoiceOps[F[_, _], A, B](fab: F[A, B])(implicit F: ProChoice[F]) {
  def left[C] : F[Either[A, C], Either[B, C]] = F.left(fab)
  def right[C]: F[Either[C, A], Either[C, B]] = F.right(fab)
}
