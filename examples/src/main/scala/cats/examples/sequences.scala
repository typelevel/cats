package cats.examples

import cats.data.Streaming
import Streaming.syntax._

object Sequences {

  // We're going to use the type `Z` to represent the integers. This
  // is partially for brevity, and partially to be clear that `BigInt`
  // is just an implementation detail.
  type Z = BigInt

  // Some useful integer constants.
  val Z0 = BigInt(0)
  val Z1 = BigInt(1)
  val Z2 = BigInt(2)
  val Z4 = BigInt(4)
  val Z10 = BigInt(10)
  val Z12 = BigInt(12)

  // Here are three implementations of the Fibonacci sequence as
  // Streaming[Z] values.
  lazy val fibs0: Streaming[Z] =
    Z0 %:: Z1 %:: (fibs0 zipMap fibs0.drop(1))(_ + _)

  lazy val fibs1: Streaming[Z] =
    Streaming.infinite((Z0, Z1)) { case (x, y) => (y, x + y) } map (_._1)

  lazy val fibs2: Streaming[Z] = {
    def term(x: Z, y: Z): Streaming[Z] = x %:: term(y, x + y)
    term(Z0, Z1)
  }

  // Similarly, we can use a pair of `Z` values to represent a
  // rational number, which we will call `Q`. We expect these values
  // to be in simplest form (i.e. `(4, 4)` would be invalid), and we
  // also require that the denominator be non-zero.
  type Q = (Z, Z)

  // Some useful rational constants.
  val Q0 = (Z0, Z1)
  val Q1 = (Z1, Z1)

  // We will define a `Streaming[Q]` value which encompasses all the
  // rational numbers using Cantor's diagonalization method. In other
  // words, for any rational number `(n, d)`, this method (given
  // sufficient time and memory) would eventually generate the value.
  //
  // In practice it takes about a million steps to generate 289/994,
  // so for numbers whose numerator or denominator are large, you
  // might have to wait a very long time.
  lazy val rats: Streaming[Q] = {
    val zs = Streaming.infinite(Z1)(_ + Z1)
    val qs = (zs product zs).filter { case (n, d) => (n gcd d) == Z1 }
    Q0 %:: qs.flatMap { case q @ (n, d) => Streaming(q, (-n, d)) }
  }

  // We can model a pseudo-random number generator as a method which
  // builds a `Streaming[Long]` from a `seed` value. This method
  // implements a linear-congruent generator (LCG) which uses the
  // parameters for Donald Knuth's MMIX RNG.
  def lcg(seed: Long): Streaming[Long] =
    Streaming.infinite(seed)(n => n * 6364136223846793005L + 1442695040888963407L)

  // If we want to get random `Int` values instead of `Long`, we can
  // do so by transforming `lcg` appropriately.
  def ints(seed: Long): Streaming[Int] =
    lcg(seed).flatMap { n =>
      val n0 = ((n >>> 32) & 0xffffffff).toInt
      val n1 = (n & 0xffffffff).toInt
      Streaming(n0, n1)
    }

  // Digits of Pi as a Streaming[Z].
  //
  // This is an example of a non-trivial algorithm modeled as an
  // instance of Streaming[Z]. If the digits were going to be used
  // many times we could memoize the stream to trade space for time.
  //
  // This approach uses continued fractions to gain ever-closer
  // approximations of Pi. The algorithm used comes from:
  //
  // http://www.cs.utsa.edu/~wagner/pi/pi_cont.html
  val pi: Streaming[Z] = {

    // This method steps through the underlying algorithm. It calls
    // emit when new digits become available.
    def loop(k: Z, a: Z, b: Z, a1: Z, b1: Z): Streaming[Z] = {
      val (p, q) = (k * k, k * Z2 + Z1)
      val (aa, bb, aa1, bb1) = (a1, b1, p * a + q * a1, p * b + q * b1)
      val (d, d1) = (aa / bb, aa1 / bb1)
      emit(k + Z1, aa, bb, aa1, bb1, p, q, d, d1)
    }

    // This method emits digits as long as they are available. When no
    // more digits are present, we call back to our loop method.
    //
    // You might worry that if emit fails to produce digits, and keeps
    // calling loop (which will recall emit) there could be a
    // possibility of an infinite loop, or at least a stack
    // overflow).
    //
    // However, the algorithm guarantees that we will get
    // at least one digit each time loop calls emit (i.e. we will be
    // in the "else block" of our code). This is important, because
    // %:: will suspend the evaluation of the tail (i.e. the next emit
    // call). We are relying on this suspension, and the stack-safety
    // of Streaming itself, to avoid stack overflows.
    def emit(k: Z, a: Z, b: Z, a1: Z, b1: Z, p: Z, q: Z, d: Z, d1: Z): Streaming[Z] =
      if (d != d1) loop(k, a, b, a1, b1) else {
        val (aa, aa1) = ((a % b) * Z10, (a1 % b1) * Z10)
        val (dd, dd1) = (aa / b, aa1 / b1)
        d %:: emit(k, aa, b, aa1, b1, p, q, dd, dd1)
      }

    // These are the starting parameters we need to calculate Pi.
    loop(Z2, Z4, Z1, Z12, Z4)
  }
}
