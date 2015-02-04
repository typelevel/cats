package cats

import simulacrum._

/**
 * Weaker version of Applicative[F]; has apply but not pure.
 *
 * Laws:
 *  - apply(apply(fa)(fab))(fbc) = apply(fa)(apply(fab)(map(fbc)(bc => ab => ab andThen bc)))
 */
@typeclass trait Apply[F[_]] extends Functor[F] { self =>
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  def apply2[A, B, Z](fa: F[A], fb: F[B])(f: F[(A, B) => Z]): F[Z] =
    apply(fa)(apply(fb)(map(f)(f => (b: B) => (a: A) => f(a, b))))

  def apply3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: F[(A, B, C) => Z]): F[Z] =
    apply(fc)(apply2(fa, fb)(map(f)(f =>
        (a: A, b: B) =>
          (c: C) =>
            f(a, b, c))))

  def apply4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: F[(A, B, C, D) => Z]): F[Z] =
    apply2(fc, fd)(apply2(fa, fb)(map(f)(f =>
        (a: A, b: B) =>
          (c: C, d: D) =>
            f(a, b, c, d))))

  def apply5[A, B, C, D, E, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: F[(A, B, C, D, E) => Z]): F[Z] =
    apply2(fd, fe)(apply3(fa, fb, fc)(map(f)(f =>
        (a: A, b: B, c: C) =>
          (d: D, e: E) =>
            f(a, b, c, d, e))))

  def apply6[A, B, C, D, E, FF, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF])
    (f: F[(A, B, C, D, E, FF) => Z]): F[Z] =
    apply3(fd, fe, ff)(apply3(fa, fb, fc)(map(f)(f =>
        (a: A, b: B, c: C) =>
          (d: D, e: E, ff: FF) =>
            f(a, b, c, d, e, ff))))

  def apply7[A, B, C, D, E, FF, G, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G])
    (f: F[(A, B, C, D, E, FF, G) => Z]): F[Z] =
    apply3(fe, ff, fg)(apply4(fa, fb, fc, fd)(map(f)(f =>
        (a: A, b: B, c: C, d: D) =>
          (e: E, ff: FF, g: G) =>
            f(a, b, c, d, e, ff, g))))

  def apply8[A, B, C, D, E, FF, G, H, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF],
    fg: F[G], fh: F[H])(f: F[(A, B, C, D, E, FF, G, H) => Z]): F[Z] =
    apply4(fe, ff, fg, fh)(apply4(fa, fb, fc, fd)(map(f)(f =>
        (a: A, b: B, c: C, d: D) =>
          (e: E, ff: FF, g: G, h: H) =>
            f(a, b, c, d, e, ff, g, h))))

  def apply9[A, B, C, D, E, FF, G, H, I, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF],
    fg: F[G], fh: F[H], fi: F[I])(f: F[(A, B, C, D, E, FF, G, H, I) => Z]): F[Z] =
    apply5(fe, ff, fg, fh, fi)(apply4(fa, fb, fc, fd)(map(f)(f =>
        (a: A, b: B, c: C, d: D) =>
          (e: E, ff: FF, g: G, h: H, i: I) =>
            f(a, b, c, d, e, ff, g, h, i))))

  def apply10[A, B, C, D, E, FF, G, H, I, J, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF],
    fg: F[G], fh: F[H], fi: F[I], fj: F[J])(f: F[(A, B, C, D, E, FF, G, H, I, J) => Z]): F[Z] =
    apply5(ff, fg, fh, fi, fj)(apply5(fa, fb, fc, fd, fe)(map(f)(f =>
        (a: A, b: B, c: C, d: D, e: E) =>
          (ff: FF, g: G, h: H, i: I, j: J) =>
            f(a, b, c, d, e, ff, g, h, i, j))))

  def apply11[A, B, C, D, E, FF, G, H, I, J, K, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF],
    fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K])(f: F[(A, B, C, D, E, FF, G, H, I, J, K) => Z]): F[Z] =
    apply6(ff, fg, fh, fi, fj, fk)(apply5(fa, fb, fc, fd, fe)(map(f)(f =>
        (a: A, b: B, c: C, d: D, e: E) =>
          (ff: FF, g: G, h: H, i: I, j: J, k: K) =>
            f(a, b, c, d, e, ff, g, h, i, j, k))))

  def apply12[A, B, C, D, E, FF, G, H, I, J, K, L, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF],
    fg: F[G], fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L])(f: F[(A, B, C, D, E, FF, G, H, I, J, K, L) => Z]): F[Z] =
    apply6(fg, fh, fi, fj, fk, fl)(apply6(fa, fb, fc, fd, fe, ff)(map(f)(f =>
        (a: A,  b: B,  c: C,  d: D,  e: E,  ff: FF) =>
          (g: G, h: H, i: I, j: J, k: K, l: L) =>
            f(a, b, c, d, e, ff, g, h, i, j, k, l))))


  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => (a: A) => f(a, b)))

  def compose[G[_]](implicit GG: Apply[G]): Apply[({type λ[α] = F[G[α]]})#λ] =
    new CompositeApply[F,G] {
      implicit def F: Apply[F] = self
      implicit def G: Apply[G] = GG
    }
}

trait CompositeApply[F[_],G[_]]
    extends Apply[λ[α => F[G[α]]]] with CompositeFunctor[F,G] {

  implicit def F: Apply[F]
  implicit def G: Apply[G]

  def apply[A,B](fa: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
    F.apply(fa)(F.map(f)(gab => G.apply(_)(gab)))
}
