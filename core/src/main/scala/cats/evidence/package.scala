package cats

package object evidence {

  /**
   * A convenient type alias for Is, which declares that A is the same
   * type as B.
   */
  type ===[A, B] = A Is B
  /** Proof that for all A[_[_] ] we have A[F] === A[G] */
  type =~=[F[_], G[_]] = F IsK G
  /** Proof that for all A[_[_,_] ] we have A[F] === A[G] */
  type =~~=[F[_,_], G[_,_]] = F IsK2 G

  type =:!=[A, B] = NotEq[A, B]

  /**
   * This type level equality represented by `Is` is referred to as
   * "Leibniz equality", and it had the name "Leibniz" in the scalaz
   *  https://en.wikipedia.org/wiki/Gottfried_Wilhelm_Leibniz
   */
  type Leibniz[A, B] = A Is B

  /**
   * A convenient type alias for As, this declares that A is a
   * subtype of B, and should be able to be  a B is
   * expected.
   */
  type <~<[-A, +B] = A As B

  /**
   * A flipped alias, for those used to their arrows running left to right
   */
  type >~>[+B, -A] = A As B

  /**
   * The property that a value of type A can be used in a context
   * expecting a B if A <~< B is referred to as the "Liskov
   * Substitution Principle", which is named for Barbara Liskov:
   * https://en.wikipedia.org/wiki/Barbara_Liskov
   */
  type Liskov[-A, +B] = A As B
}
