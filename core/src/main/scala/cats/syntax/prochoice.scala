package cats.syntax

import cats.data.Xor
import cats.functor.ProChoice

trait ProChoiceSyntax {
  // TODO: use simulacrum instances eventually
  implicit def proChoiceSyntax[F[_, _]: ProChoice, A, B](fab: F[A, B]): ProChoiceOps[F, A, B] =
    new ProChoiceOps[F, A, B](fab)
}

class ProChoiceOps[F[_, _], A, B](fab: F[A, B])(implicit F: ProChoice[F]) {
  def left[C] : F[A Xor C, B Xor C] = F.left(fab)
  def right[C]: F[C Xor A, C Xor B] = F.right(fab)
}
