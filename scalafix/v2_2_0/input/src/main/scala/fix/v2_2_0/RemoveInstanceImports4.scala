/*
rule = "scala:fix.v2_2_0.RemoveInstanceImports"
 */
package fix
package to2_2_0

import cats.implicits._
import cats.Order

sealed trait Resolver extends Product with Serializable {
  import Resolver._

  val path: String = {
    val url = this match {
      case MavenRepository(_, location, _) => location
      case IvyRepository(_, pattern, _)    => pattern.takeWhile(!Set('[', '(')(_))
    }
    url.replace(":", "")
  }
}

object Resolver {
  final case class Credentials(user: String, pass: String)

  final case class MavenRepository(name: String, location: String, credentials: Option[Credentials])
      extends Resolver
  final case class IvyRepository(name: String, pattern: String, credentials: Option[Credentials])
      extends Resolver

  val mavenCentral: MavenRepository =
    MavenRepository("public", "https://repo1.maven.org/maven2/", None)

  implicit val resolverOrder: Order[Resolver] =
    Order.by {
      case MavenRepository(name, location, _) => (1, name, location)
      case IvyRepository(name, pattern, _)    => (2, name, pattern)
    }
}

final case class Scope[A](value: A, resolvers: List[Resolver])

object Scope {

  def combineByResolvers[A: Order](scopes: List[Scope[List[A]]]): List[Scope[List[A]]] =
    scopes.groupByNel(_.resolvers).toList.map {
      case (resolvers, group) => Scope(group.reduceMap(_.value).distinct.sorted, resolvers)
    }

  implicit def scopeOrder[A: Order]: Order[Scope[A]] =
    Order.by((scope: Scope[A]) => (scope.value, scope.resolvers))
}
