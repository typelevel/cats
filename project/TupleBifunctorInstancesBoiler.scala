import sbt._

import Boilerplate._
import Boilerplate.{Template, TemplateVals}
import sbt.File

object GenTupleBifunctorInstances extends Template {
  override def range = 2 to 11
  override def filename(root: sbt.File): File =
    root / "cats" / "instances" / "NTupleBifunctorInstances.scala"

  override def content(tv: TemplateVals): String = {
    import tv._

    block"""
    |
    |package cats
    |package instances
    |
    |private[cats] trait NTupleBifunctorInstances {
    |
    |  private def instance[F[_, _]](bim: (F[Any, Any], Any => Any, Any => Any) => F[Any, Any]): Bifunctor[F] =
    |    new Bifunctor[F] {
    |      def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] =
    |        bim(fab.asInstanceOf[F[Any, Any]], f.asInstanceOf[Any => Any], g.asInstanceOf[Any => Any]).asInstanceOf[F[C, D]]
    |    }
    -
    -  implicit final def catsStdBifunctorForTuple$arity${`[A0, A(N - 2)]`}: Bifunctor[${`(A..N - 2, *, *)`}] =
    -    instance[${`(A..N - 2, *, *)`}] { (fab, f, g) =>
    -      fab.copy(_${arity - 1} = f(fab._${arity - 1}), _$arity = g(fab._$arity))
    -    }
    |}"""
  }
}
