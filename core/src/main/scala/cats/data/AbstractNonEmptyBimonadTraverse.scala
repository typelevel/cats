package cats
package data

abstract private[data] class AbstractNonEmptyBimonadTraverse[F[_], NonEmptyF[_]](implicit MF: Monad[F],
                                                                                 CF: CoflatMap[F],
                                                                                 TF: Traverse[F])
    extends Bimonad[NonEmptyF]
    with NonEmptyTraverse[NonEmptyF] {
  val monadInstance = MF.asInstanceOf[Monad[NonEmptyF]]
  val coflatMapInstance = CF.asInstanceOf[CoflatMap[NonEmptyF]]
  val traverseInstance = Traverse[F].asInstanceOf[Traverse[NonEmptyF]]

  def pure[A](x: A): NonEmptyF[A] = monadInstance.pure(x)

  override def map[A, B](fa: NonEmptyF[A])(f: A => B): NonEmptyF[B] = monadInstance.map(fa)(f)

  def flatMap[A, B](fa: NonEmptyF[A])(f: A => NonEmptyF[B]): NonEmptyF[B] =
    monadInstance.flatMap(fa)(f)

  override def map2[A, B, Z](fa: NonEmptyF[A], fb: NonEmptyF[B])(f: (A, B) => Z): NonEmptyF[Z] =
    monadInstance.map2(fa, fb)(f)

  override def map2Eval[A, B, Z](fa: NonEmptyF[A], fb: Eval[NonEmptyF[B]])(f: (A, B) => Z): Eval[NonEmptyF[Z]] =
    monadInstance.map2Eval(fa, fb)(f)

  def coflatMap[A, B](fa: NonEmptyF[A])(f: NonEmptyF[A] => B): NonEmptyF[B] =
    coflatMapInstance.coflatMap(fa)(f)

  def tailRecM[A, B](a: A)(f: A => NonEmptyF[Either[A, B]]): NonEmptyF[B] =
    monadInstance.tailRecM(a)(f)

  def foldLeft[A, B](fa: NonEmptyF[A], b: B)(f: (B, A) => B): B =
    traverseInstance.foldLeft(fa, b)(f)

  def foldRight[A, B](fa: NonEmptyF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    traverseInstance.foldRight(fa, lb)(f)

  override def foldMap[A, B](fa: NonEmptyF[A])(f: A => B)(implicit B: Monoid[B]): B =
    traverseInstance.foldMap(fa)(f)

  override def traverse[G[_], A, B](fa: NonEmptyF[A])(f: A => G[B])(implicit G: Applicative[G]): G[NonEmptyF[B]] =
    traverseInstance.traverse(fa)(f)

  override def mapWithIndex[A, B](fa: NonEmptyF[A])(f: (A, Int) => B): NonEmptyF[B] =
    traverseInstance.mapWithIndex(fa)(f)

  override def zipWithIndex[A](fa: NonEmptyF[A]): NonEmptyF[(A, Int)] = traverseInstance.zipWithIndex(fa)

  override def exists[A](fa: NonEmptyF[A])(p: A => Boolean): Boolean = traverseInstance.exists(fa)(p)

  override def forall[A](fa: NonEmptyF[A])(p: A => Boolean): Boolean = traverseInstance.forall(fa)(p)

  override def get[A](fa: NonEmptyF[A])(idx: Long): Option[A] = traverseInstance.get(fa)(idx)

  override def isEmpty[A](fa: NonEmptyF[A]): Boolean = false

  override def foldM[G[_], A, B](fa: NonEmptyF[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
    traverseInstance.foldM(fa, z)(f)

  override def fold[A](fa: NonEmptyF[A])(implicit A: Monoid[A]): A = traverseInstance.fold(fa)

  override def toList[A](fa: NonEmptyF[A]): List[A] = traverseInstance.toList(fa)

  override def reduceLeftOption[A](fa: NonEmptyF[A])(f: (A, A) => A): Option[A] =
    traverseInstance.reduceLeftOption(fa)(f)

  override def find[A](fa: NonEmptyF[A])(f: A => Boolean): Option[A] = traverseInstance.find(fa)(f)

  override def collectFirst[A, B](fa: NonEmptyF[A])(pf: PartialFunction[A, B]): Option[B] =
    traverseInstance.collectFirst(fa)(pf)

  override def collectFirstSome[A, B](fa: NonEmptyF[A])(f: A => Option[B]): Option[B] =
    traverseInstance.collectFirstSome(fa)(f)

}
