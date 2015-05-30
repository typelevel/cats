package cats
package tests

class FoldableTestsAdditional extends CatsSuite {
  import Fold.{Continue, Return, Pass}

  // disable scalatest ===
  override def convertToEqualizer[T](left: T): Equalizer[T] = ???

  // exists method written in terms of foldRight
  def exists[F[_]: Foldable, A: Eq](as: F[A], goal: A): Lazy[Boolean] =
    Foldable[F].foldRight(as, Lazy(false)) { a =>
      if (a === goal) Return(true) else Pass
    }

  test("Foldable[List]") {
    val F = Foldable[List]

    // some basic sanity checks
    val ns = (1 to 10).toList
    val total = ns.sum
    assert(F.foldLeft(ns, 0)(_ + _) == total)
    assert(F.foldRight(ns, Lazy(0))(x => Continue(x + _)).value == total)
    assert(F.fold(ns) == total)

    // more basic checks
    val names = List("Aaron", "Betty", "Calvin", "Deirdra")
    assert(F.foldMap(names)(_.length) == names.map(_.length).sum)

    // test trampolining
    val large = (1 to 10000).toList
    assert(exists(large, 10000).value)

    // safely build large lists
    val larger = F.foldRight(large, Lazy(List.empty[Int]))(x => Continue((x + 1) :: _))
    assert(larger.value == large.map(_ + 1))
  }

  test("Foldable[Stream]") {
    val F = Foldable[Stream]

    def bomb[A]: A = sys.error("boom")
    val dangerous = 0 #:: 1 #:: 2 #:: bomb[Stream[Int]]

    // doesn't blow up - this also ensures it works for infinite streams.
    assert(exists(dangerous, 2).value)

    // lazy results don't blow up unless you call .value on them.
    val doom: Lazy[Boolean] = exists(dangerous, -1)

    // ensure that the Lazy[B] param to foldRight is actually being
    // handled lazily. it only needs to be evaluated if we reach the
    // "end" of the fold.
    val trap = Lazy(bomb[Boolean])
    val result = F.foldRight(1 #:: 2 #:: Stream.Empty, trap) { n =>
      if (n == 2) Return(true) else Pass
    }
    assert(result.value)
  }
}
