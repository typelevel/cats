package cats
package data


final class IndexedStoreT[F[_], I, A, B](val run: (F[A => B], I)) extends Serializable {
  import StoreT._

  def xmap[X1, X2](f: I => X1)(g: X2 => A)(implicit  F: Functor[F]): IndexedStoreT[F, X1, X2, B] =
    indexedStoreT((F.map(set)(_ compose g), f(pos)))

  def imap[X](f: I => X): IndexedStoreT[F, X, A, B] =
    indexedStoreT((set, f(pos)))

  def contramap[X](g: X => A)(implicit F: Functor[F]): IndexedStoreT[F, I, X, B] =
    indexedStoreT((F.map(set)(_ compose g), pos))

  def bimap[X, Y](f: I => X, g: B => Y)(implicit  F: Functor[F]): IndexedStoreT[F, X, A, Y] =
    indexedStoreT((F.map(set)(g.compose), f(pos)))

  def leftMap[X](f: I => X): IndexedStoreT[F, X, A, B] =
    imap(f)

  def map[C](f: B => C)(implicit  F: Functor[F]): IndexedStoreT[F, I, A, C] =
    indexedStoreT(mapRun(g => f compose g))

  private def mapRun[C](f: (A => B) => C)(implicit  F: Functor[F]): (F[C], I) =
    (F.map(run._1)(f), run._2)

  def put(a: A)(implicit  F: Functor[F]): F[B] =
    F.map(run._1)(_(a))

  def puts(f: I => A)(implicit  F: Functor[F]): F[B] =
    put(f(pos))

  def putf[G[_]](a: G[A])(implicit  F: Functor[F], G: Functor[G]): G[F[B]] =
    G.map(a)(put)

  def putsf[G[_]](f: I => G[A])(implicit F: Functor[F], G: Functor[G]): G[F[B]]  =
    putf(f(pos))

  def set: F[A => B] = run._1

  def pos: I = run._2

  def peek(a: A)(implicit F: Comonad[F]): B =
    F.extract(set)(a)

  def peeks(f: I => A)(implicit F: Comonad[F]): B =
    F.extract(set)(f(pos))

  def seek[J](j: J): IndexedStoreT[F, J, A, B] =
    indexedStoreT((set, j))

  def seeks[J](f: I => J): IndexedStoreT[F, J, A, B] =
    indexedStoreT((set, f(pos)))

  def extract(implicit F: Comonad[F], ev: I <:< A): B = F.extract(run._1)(run._2)

  def duplicate[J](implicit F: CoflatMap[F]): IndexedStoreT[F, I, J, IndexedStoreT[F, J, A, B]] =
    indexedStoreT((F.coflatMap(run._1)(ff => (a: J) => indexedStoreT((ff, a))), pos))

  def coflatMap[K, C](f: IndexedStoreT[F, K, A, B] => C)(implicit F: CoflatMap[F]): IndexedStoreT[F, I, K, C] =
    indexedStoreT((F.coflatMap(run._1)(ff => (a: K) => f(indexedStoreT((ff, a)))), pos))

}

object IndexedStoreT extends StoreTInstances with CommonStoreTConstructors {
  def applyF[F[_], I, A, B](runF: (F[A => B], I)): IndexedStoreT[F, I, A, B] =
    new IndexedStoreT(runF)
}

private[data] trait CommonStoreTConstructors {

  def indexedStoreT[F[_], I, A, B](r: (F[A => B], I)): IndexedStoreT[F, I, A, B] =
    new IndexedStoreT(r)

}

abstract private[data] class StoreTFunctions extends CommonStoreTConstructors {

  def apply[F[_], A, B](r: (F[A => B], A)): StoreT[F, A, B] =
    indexedStoreT[F, A, A, B](r)

  def storeT[F[_], A, B](r: (F[A => B], A)): StoreT[F, A, B] =
    indexedStoreT[F, A, A, B](r)
}

abstract private[data] class StoreFunctions {

  def apply[A, B](f:A => B, a: A): StoreId[A, B] =
    IndexedStoreT.applyF((Now(f),a))

}


sealed abstract private[data] class IndexedStoreTFunctorLeft[F[_], A0, B0] extends Functor[IndexedStoreT[F, ?, A0, B0]] {
  override def map[A, B](fa: IndexedStoreT[F, A, A0, B0])(f: A => B): IndexedStoreT[F, B, A0, B0] =
    fa leftMap f
}

sealed abstract private[data] class IndexedStoreTFunctor[F[_], I0, A0] extends Functor[IndexedStoreT[F, I0, A0, ?]] {
  implicit def F: Functor[F]

  override def map[A, B](fa: IndexedStoreT[F, I0, A0, A])(f: A => B): IndexedStoreT[F, I0, A0, B] =
    fa map f
}

sealed abstract private[data] class IndexedStoreTContravariant[F[_], I0, B0]
  extends Contravariant[IndexedStoreT[F, I0, ?, B0]] {
  implicit def F: Functor[F]

  override def contramap[A, B](fa: IndexedStoreT[F, I0, A, B0])(f: B => A): IndexedStoreT[F, I0, B, B0] =
    fa.contramap(f)
}

sealed abstract private[data] class IndexedStoreTBifunctor[F[_], A0] extends Bifunctor[IndexedStoreT[F, ?, A0, ?]] {
  implicit def F: Functor[F]

  override def bimap[A, B, C, D](fab: IndexedStoreT[F, A, A0, B])(f: A => C, g: B => D): IndexedStoreT[F, C, A0, D] =
    fab.bimap(f, g)
}

sealed abstract private[data] class StoreTCoflatMap[F[_], A0] extends IndexedStoreTFunctor[F, A0, A0] with CoflatMap[StoreT[F, A0, ?]]{
  implicit def F: CoflatMap[F]

  override def coflatMap[A, B](fab: StoreT[F, A0, A])(f: StoreT[F, A0, A]=> B): StoreT[F, A0, B] =
    fab coflatMap f
}

sealed abstract private[data] class StoreTComonad[F[_], A0] extends StoreTCoflatMap[F, A0] with Comonad[StoreT[F, A0, ?]]{
  implicit def F: Comonad[F]

  override def coflatten[A](fa: StoreT[F, A0, A]): StoreT[F, A0, StoreT[F, A0, A]] = fa.duplicate
  override def extract[A](p: StoreT[F, A0, A]): A =
    p.extract
}

sealed abstract private[data] class IndexedStoreTInstances2 {
  implicit def catsDataContravariantForIndexedStoreT[F[_], I, B](implicit F0: Functor[F]): Contravariant[IndexedStoreT[F, I, ?, B]] =
    new IndexedStoreTContravariant[F, I, B] { implicit def F = F0 }
}

sealed abstract private[data] class IndexedStoreTInstances1 extends IndexedStoreTInstances2 {
  implicit def catsDataFunctorLeftForIndexedStoreT[F[_], A, B]: Functor[IndexedStoreT[F, ?, A, B]] =
    new IndexedStoreTFunctorLeft[F, A, B] {}
}

sealed abstract private[data] class IndexedStoreTInstances0 extends IndexedStoreTInstances1 {
  implicit def catsDataBifunctorForIndexedStoreT[F[_], A](implicit F0: Functor[F]): Bifunctor[IndexedStoreT[F, ?, A, ?]] =
    new IndexedStoreTBifunctor[F, A] { implicit def F = F0 }
}

sealed abstract private[data] class IndexedStoreTInstances extends IndexedStoreTInstances0 {
  implicit def catsDataFunctorRightForIndexedStoreT[F[_], I, A](implicit F0: Functor[F]): Functor[IndexedStoreT[F, I, A, ?]] =
    new IndexedStoreTFunctor[F, I, A] { implicit def F = F0 }
}

sealed abstract private[data] class StoreTInstances1 extends IndexedStoreTInstances {
  implicit def catsDataCoflatMapForStoreT[F[_], A](implicit F0: CoflatMap[F]): CoflatMap[StoreT[F, A, ?]] =
    new StoreTCoflatMap[F, A] { implicit def F = F0}
}

sealed abstract private[data] class StoreTInstances extends StoreTInstances1 {
  implicit def catsDataComonadForStoreT[F[_], A](implicit F0: Comonad[F]): Comonad[StoreT[F, A, ?]] =
    new StoreTComonad[F, A] { implicit def F = F0}
}
