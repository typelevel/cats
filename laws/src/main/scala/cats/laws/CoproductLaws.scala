package cats.laws

/**
  * Laws that must be obeyed by any `Coproduct`.
  *
  * Does this makes sense so... extends MonadLaws[Coproduct[F, G, ?]]
  * with ComonadLaws[Coproduct[F, G, ?]]
  * with TraverseLaws[Coproduct[F, G, ?]... ?
  */
trait CoproductLaws[F[_], G[_], A]
