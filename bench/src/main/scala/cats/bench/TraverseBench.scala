package cats.bench

import cats.{Always, Applicative, Eval, Traverse}
import cats.instances.either._
import cats.instances.list._
import cats.instances.vector._
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class TraverseListBench {
  val instance: Traverse[List] = Traverse[List]
  val f: Int => Either[Int, Int] = Right(_)

  val xs1: List[Int] = (1 to 10).toList
  val xs2: List[Int] = (1 to 100).toList
  val xs3: List[Int] = (1 to 1000).toList
  val xs4: List[Int] = (1 to 10000).toList

  def traverseCats[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
    instance
      .foldRight[A, G[List[B]]](fa, Always(G.pure(List.empty))) { (a, lglb) =>
        G.map2Eval(f(a), lglb)(_ :: _)
      }
      .value

  def traverseFoldRight[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
    fa.foldRight[Eval[G[List[B]]]](Always(G.pure(Nil))) {
        case (h, t) => G.map2Eval(f(h), Eval.defer(t))(_ :: _)
      }
      .value

  def traverseRec[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
    def loop(fa: List[A]): Eval[G[List[B]]] = fa match {
      case h :: t => G.map2Eval(f(h), Eval.defer(loop(t)))(_ :: _)
      case Nil    => Eval.now(G.pure(Nil))
    }
    loop(fa).value
  }

  @Benchmark def traverseCats1: Either[Int, List[Int]] = traverseCats(xs1)(f)
  @Benchmark def traverseCats2: Either[Int, List[Int]] = traverseCats(xs2)(f)
  @Benchmark def traverseCats3: Either[Int, List[Int]] = traverseCats(xs3)(f)
  @Benchmark def traverseCats4: Either[Int, List[Int]] = traverseCats(xs4)(f)

  @Benchmark def traverseFoldRight1: Either[Int, List[Int]] = traverseFoldRight(xs1)(f)
  @Benchmark def traverseFoldRight2: Either[Int, List[Int]] = traverseFoldRight(xs2)(f)
  @Benchmark def traverseFoldRight3: Either[Int, List[Int]] = traverseFoldRight(xs3)(f)
  @Benchmark def traverseFoldRight4: Either[Int, List[Int]] = traverseFoldRight(xs4)(f)

  @Benchmark def traverseRec1: Either[Int, List[Int]] = traverseRec(xs1)(f)
  @Benchmark def traverseRec2: Either[Int, List[Int]] = traverseRec(xs2)(f)
  @Benchmark def traverseRec3: Either[Int, List[Int]] = traverseRec(xs3)(f)
  @Benchmark def traverseRec4: Either[Int, List[Int]] = traverseRec(xs4)(f)
}

@State(Scope.Benchmark)
class TraverseVectorBench {
  val instance: Traverse[Vector] = Traverse[Vector]
  val f: Int => Either[Int, Int] = Right(_)

  val xs1: Vector[Int] = (1 to 10).toVector
  val xs2: Vector[Int] = (1 to 100).toVector
  val xs3: Vector[Int] = (1 to 1000).toVector
  val xs4: Vector[Int] = (1 to 10000).toVector

  def traverseCats[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] =
    instance
      .foldRight[A, G[Vector[B]]](fa, Always(G.pure(Vector.empty))) { (a, lgvb) =>
        G.map2Eval(f(a), lgvb)(_ +: _)
      }
      .value

  def traverseFoldRight[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] =
    fa.foldRight[Eval[G[Vector[B]]]](Always(G.pure(Vector.empty))) {
        case (h, t) => G.map2Eval(f(h), Eval.defer(t))(_ +: _)
      }
      .value

  def traverseRec[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] = {
    def loop(i: Int): Eval[G[Vector[B]]] =
      if (i < fa.length) G.map2Eval(f(fa(i)), Eval.defer(loop(i + 1)))(_ +: _) else Eval.now(G.pure(Vector.empty))
    loop(0).value
  }

  def traverseIter[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] = {
    var i = fa.length - 1
    var current: Eval[G[Vector[B]]] = Eval.now(G.pure(Vector.empty))

    while (i >= 0) {
      current = G.map2Eval(f(fa(i)), current)(_ +: _)
      i -= 1
    }

    current.value
  }

  def traverseFoldRightViaList[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] =
    G.map(
      fa.foldRight[Eval[G[List[B]]]](Always(G.pure(Nil))) {
          case (h, t) => G.map2Eval(f(h), Eval.defer(t))(_ :: _)
        }
        .value
    )(_.toVector)

  def traverseRecViaList[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] = {
    def loop(i: Int): Eval[G[List[B]]] =
      if (i < fa.length) G.map2Eval(f(fa(i)), Eval.defer(loop(i + 1)))(_ :: _) else Eval.now(G.pure(Nil))
    G.map(loop(0).value)(_.toVector)
  }

  def traverseIterViaList[G[_], A, B](fa: Vector[A])(f: A => G[B])(implicit G: Applicative[G]): G[Vector[B]] = {
    var i = fa.length - 1
    var current: Eval[G[List[B]]] = Eval.now(G.pure(Nil))

    while (i >= 0) {
      current = G.map2Eval(f(fa(i)), current)(_ :: _)
      i -= 1
    }

    G.map(current.value)(_.toVector)
  }

  @Benchmark def traverseCats1: Either[Int, Vector[Int]] = traverseCats(xs1)(f)
  @Benchmark def traverseCats2: Either[Int, Vector[Int]] = traverseCats(xs2)(f)
  @Benchmark def traverseCats3: Either[Int, Vector[Int]] = traverseCats(xs3)(f)
  @Benchmark def traverseCats4: Either[Int, Vector[Int]] = traverseCats(xs4)(f)

  @Benchmark def traverseFoldRight1: Either[Int, Vector[Int]] = traverseFoldRight(xs1)(f)
  @Benchmark def traverseFoldRight2: Either[Int, Vector[Int]] = traverseFoldRight(xs2)(f)
  @Benchmark def traverseFoldRight3: Either[Int, Vector[Int]] = traverseFoldRight(xs3)(f)
  @Benchmark def traverseFoldRight4: Either[Int, Vector[Int]] = traverseFoldRight(xs4)(f)

  @Benchmark def traverseRec1: Either[Int, Vector[Int]] = traverseRec(xs1)(f)
  @Benchmark def traverseRec2: Either[Int, Vector[Int]] = traverseRec(xs2)(f)
  @Benchmark def traverseRec3: Either[Int, Vector[Int]] = traverseRec(xs3)(f)
  @Benchmark def traverseRec4: Either[Int, Vector[Int]] = traverseRec(xs4)(f)

  @Benchmark def traverseIter1: Either[Int, Vector[Int]] = traverseIter(xs1)(f)
  @Benchmark def traverseIter2: Either[Int, Vector[Int]] = traverseIter(xs2)(f)
  @Benchmark def traverseIter3: Either[Int, Vector[Int]] = traverseIter(xs3)(f)
  @Benchmark def traverseIter4: Either[Int, Vector[Int]] = traverseIter(xs4)(f)

  @Benchmark def traverseFoldRightViaList1: Either[Int, Vector[Int]] = traverseFoldRightViaList(xs1)(f)
  @Benchmark def traverseFoldRightViaList2: Either[Int, Vector[Int]] = traverseFoldRightViaList(xs2)(f)
  @Benchmark def traverseFoldRightViaList3: Either[Int, Vector[Int]] = traverseFoldRightViaList(xs3)(f)
  @Benchmark def traverseFoldRightViaList4: Either[Int, Vector[Int]] = traverseFoldRightViaList(xs4)(f)

  @Benchmark def traverseRecViaList1: Either[Int, Vector[Int]] = traverseRecViaList(xs1)(f)
  @Benchmark def traverseRecViaList2: Either[Int, Vector[Int]] = traverseRecViaList(xs2)(f)
  @Benchmark def traverseRecViaList3: Either[Int, Vector[Int]] = traverseRecViaList(xs3)(f)
  @Benchmark def traverseRecViaList4: Either[Int, Vector[Int]] = traverseRecViaList(xs4)(f)

  @Benchmark def traverseIterViaList1: Either[Int, Vector[Int]] = traverseIterViaList(xs1)(f)
  @Benchmark def traverseIterViaList2: Either[Int, Vector[Int]] = traverseIterViaList(xs2)(f)
  @Benchmark def traverseIterViaList3: Either[Int, Vector[Int]] = traverseIterViaList(xs3)(f)
  @Benchmark def traverseIterViaList4: Either[Int, Vector[Int]] = traverseIterViaList(xs4)(f)
}
