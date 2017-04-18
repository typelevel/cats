package cats.laws.discipline


import org.scalacheck.{Arbitrary, Cogen, Prop}
import Prop.forAll
import cats.{Applicative, Eq, Monoid, Traverse1}
import cats.laws.Traverse1Laws


trait Traverse1Tests[F[_]] extends TraverseTests[F] with ReducibleTests[F] {
  def laws: Traverse1Laws[F]

  def traverse1[G[_]: Applicative, A: Arbitrary, B: Arbitrary, C: Arbitrary, M: Arbitrary, X[_]: Applicative, Y[_]: Applicative](implicit
                                                                                                             ArbFA: Arbitrary[F[A]],
                                                                                                             ArbXB: Arbitrary[X[B]],
                                                                                                             ArbYB: Arbitrary[Y[B]],
                                                                                                             ArbYC: Arbitrary[Y[C]],
                                                                                                             ArbFB: Arbitrary[F[B]],
                                                                                                             ArbFGA: Arbitrary[F[G[A]]],
                                                                                                             ArbGB: Arbitrary[G[B]],
                                                                                                             CogenA: Cogen[A],
                                                                                                             CogenB: Cogen[B],
                                                                                                             CogenC: Cogen[C],
                                                                                                             CogenM: Cogen[M],
                                                                                                             M: Monoid[M],
                                                                                                             MB: Monoid[B],
                                                                                                             EqFA: Eq[F[A]],
                                                                                                             EqFC: Eq[F[C]],
                                                                                                             EqG: Eq[G[Unit]],
                                                                                                             EqM: Eq[M],
                                                                                                             EqA: Eq[A],
                                                                                                             EqB: Eq[B],
                                                                                                             EqXYFC: Eq[X[Y[F[C]]]],
                                                                                                             EqXFB: Eq[X[F[B]]],
                                                                                                             EqYFB: Eq[Y[F[B]]],
                                                                                                             EqOptionA: Eq[Option[A]]
                                                                                                            ): RuleSet = {
    implicit def EqXFBYFB : Eq[(X[F[B]], Y[F[B]])] = new Eq[(X[F[B]], Y[F[B]])] {
      override def eqv(x: (X[F[B]], Y[F[B]]), y: (X[F[B]], Y[F[B]])): Boolean =
        EqXFB.eqv(x._1, y._1) && EqYFB.eqv(x._2, y._2)
    }
    new RuleSet {
      def name: String = "traverse1"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Seq(traverse[A, B, C, M, X, Y], reducible[G, A, B])
      def props: Seq[(String, Prop)] = Seq(
        "traverse1 identity" -> forAll(laws.traverse1Identity[A, C] _),
        "traverse1 sequential composition" -> forAll(laws.traverse1SequentialComposition[A, B, C, X, Y] _),
        "traverse1 parallel composition" -> forAll(laws.traverse1ParallelComposition[A, B, X, Y] _),
        "traverse1 derive reduceMap" -> forAll(laws.reduceMapDerived[A, M] _)
      )
    }
  }
}

object Traverse1Tests {
  def apply[F[_]: Traverse1]: Traverse1Tests[F] =
    new Traverse1Tests[F] { def laws: Traverse1Laws[F] = Traverse1Laws[F] }
}