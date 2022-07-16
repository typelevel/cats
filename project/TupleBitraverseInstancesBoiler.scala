import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleBitraverseInstances extends Template {
  override def range = 2 to 11
  override def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleBitraverseInstances.scala"

  override def content(tv: TemplateVals): String = {
    import tv._

    block"""
    |
    |package cats
    |package instances
    |
    |private[cats] trait NTupleBitraverseInstances {
    |  protected type γ[_]
    |
    |  private def instance[F[_, _] <: Product](
    |    bitrav: (F[Any, Any], Applicative[γ], Any => γ[Any], Any => γ[Any]) => γ[F[Any, Any]]
    |  ): Bitraverse[F] = new Bitraverse[F] {
    |    def bitraverse[G[_], A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D])(implicit G: Applicative[G]): G[F[C, D]] =
    |      bitrav(
    |        fab.asInstanceOf[F[Any, Any]],
    |        G.asInstanceOf[Applicative[γ]],
    |        f.asInstanceOf[Any => γ[Any]],
    |        g.asInstanceOf[Any => γ[Any]]
    |      ).asInstanceOf[G[F[C, D]]]
    |
    |    @inline private def last1[A, B](fab: F[A, B]): A =
    |      fab.productElement(fab.productArity - 2).asInstanceOf[A]
    |    @inline private def last2[A, B](fab: F[A, B]): B =
    |      fab.productElement(fab.productArity - 1).asInstanceOf[B]
    |    def bifoldLeft[A, B, C](fab: F[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    |      g(f(c, last1(fab)), last2(fab))
    |    def bifoldRight[A, B, C](fab: F[A, B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
    |      g(last2(fab), f(last1(fab), c))
    |  }
    -
    -  implicit final def catsStdBitraverseForTuple$arity${`[A0, A(N - 2)]`}: Bitraverse[${`(A..N - 2, *, *)`}] =
    -    instance { (fab, G, f, g) =>
    -      G.map2(f(fab._${arity - 1}), g(fab._$arity)) { (x, y) =>
    -        fab.copy(_${arity - 1} = x, _$arity = y)
    -      }
    -    }
    |}"""
  }
}
