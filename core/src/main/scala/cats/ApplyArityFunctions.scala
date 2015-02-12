package cats

trait ApplyArityFunctions[F[_]] { self: Apply[F] =>
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

  def map3[A, B, C, Z](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] =
    apply(fa)(map2(fb, fc)((b, c) => a => f(a, b, c)))

  def map4[A, B, C, D, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => Z): F[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) { case ((a, b), (c, d)) => f(a, b, c, d) }

  def map5[A, B, C, D, E, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E])(f: (A, B, C, D, E) => Z): F[Z] =
    map2(tuple2(fa, fb), tuple3(fc, fd, fe)) { case ((a, b), (c, d, e)) => f(a, b, c, d, e) }

  def map6[A, B, C, D, E, FF, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF])
    (f: (A, B, C, D, E, FF) => Z): F[Z] =
      map2(tuple3(fa, fb, fc), tuple3(fd, fe, ff)) {
        case ((a, b, c), (d, e, ff)) => f(a, b, c, d, e, ff) }

  def map7[A, B, C, D, E, FF, G, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G])
    (f: (A, B, C, D, E, FF, G) => Z): F[Z] =
      map2(tuple3(fa, fb, fc), tuple4(fd, fe, ff, fg)) {
        case ((a, b, c), (d, e, ff, g)) => f(a, b, c, d, e, ff, g) }

  def map8[A, B, C, D, E, FF, G, H, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G],
    fh: F[H])(f: (A, B, C, D, E, FF, G, H) => Z): F[Z] =
      map2(tuple4(fa, fb, fc, fd), tuple4(fe, ff, fg, fh)) {
        case ((a, b, c, d), (e, ff, g, h)) => f(a, b, c, d, e, ff, g, h) }

  def map9[A, B, C, D, E, FF, G, H, I, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G],
    fh: F[H], fi: F[I])(f: (A, B, C, D, E, FF, G, H, I) => Z): F[Z] =
      map2(tuple4(fa, fb, fc, fd), tuple5(fe, ff, fg, fh, fi)) {
        case ((a, b, c, d), (e, ff, g, h, i)) => f(a, b, c, d, e, ff, g, h, i) }

  def map10[A, B, C, D, E, FF, G, H, I, J, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G],
    fh: F[H], fi: F[I], fj: F[J])(f: (A, B, C, D, E, FF, G, H, I, J) => Z): F[Z] =
      map2(tuple5(fa, fb, fc, fd, fe), tuple5(ff, fg, fh, fi, fj)) {
        case ((a, b, c, d, e), (ff, g, h, i, j)) => f(a, b, c, d, e, ff, g, h, i, j) }

  def map11[A, B, C, D, E, FF, G, H, I, J, K, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G],
    fh: F[H], fi: F[I], fj: F[J], fk: F[K])(f: (A, B, C, D, E, FF, G, H, I, J, K) => Z): F[Z] =
      map2(tuple5(fa, fb, fc, fd, fe), tuple6(ff, fg, fh, fi, fj, fk)) {
        case ((a, b, c, d, e), (ff, g, h, i, j, k)) => f(a, b, c, d, e, ff, g, h, i, j, k) }

  def map12[A, B, C, D, E, FF, G, H, I, J, K, L, Z](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G],
    fh: F[H], fi: F[I], fj: F[J], fk: F[K], fl: F[L])(f: (A, B, C, D, E, FF, G, H, I, J, K, L) => Z): F[Z] =
      map2(tuple6(fa, fb, fc, fd, fe, ff), tuple6(fg, fh, fi, fj, fk, fl)) {
        case ((a, b, c, d, e, ff), (g, h, i, j, k, l)) => f(a, b, c, d, e, ff, g, h, i, j, k, l) }

  def tuple2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def tuple3[A, B, C](fa: F[A], fb: F[B], fc: F[C]): F[(A, B, C)] =
    map3(fa, fb, fc)((_, _, _))

  def tuple4[A, B, C, D](fa: F[A], fb: F[B], fc: F[C], fd: F[D]): F[(A, B, C, D)] =
    map4(fa, fb, fc, fd)((_, _, _, _))

  def tuple5[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E]): F[(A, B, C, D, E)] =
    map5(fa, fb, fc, fd, fe)((_, _, _, _, _))

  def tuple6[A, B, C, D, E, FF](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF]): F[(A, B, C, D, E, FF)] =
    map6(fa, fb, fc, fd, fe, ff)((_, _, _, _, _, _))

  def tuple7[A, B, C, D, E, FF, G](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], ff: F[FF], fg: F[G]): F[(A, B, C, D, E, FF, G)] =
    map7(fa, fb, fc, fd, fe, ff, fg)((_, _, _, _, _, _, _))

  def tuple8[A, B, C, D, E, G, H, I](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H],
    fi: F[I]): F[(A, B, C, D, E, G, H, I)] = map8(fa, fb, fc, fd, fe, fg, fh, fi)((_, _, _, _, _, _, _, _))

  def tuple9[A, B, C, D, E, G, H, I, J](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H],
    fi: F[I], fj: F[J]): F[(A, B, C, D, E, G, H, I, J)] = map9(fa, fb, fc, fd, fe, fg, fh, fi, fj)((_, _, _, _, _, _, _, _, _))

  def tuple10[A, B, C, D, E, G, H, I, K, J](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H],
    fi: F[I], fj: F[J], fk: F[K]): F[(A, B, C, D, E, G, H, I, J, K)] =
      map10(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk)((_, _, _, _, _, _, _, _, _, _))

  def tuple11[A, B, C, D, E, G, H, I, K, L, J](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H],
    fi: F[I], fj: F[J], fk: F[K], fl: F[L]): F[(A, B, C, D, E, G, H, I, J, K, L)] =
      map11(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl)((_, _, _, _, _, _, _, _, _, _, _))

  def tuple12[A, B, C, D, E, G, H, I, K, L, M, J](fa: F[A], fb: F[B], fc: F[C], fd: F[D], fe: F[E], fg: F[G], fh: F[H],
    fi: F[I], fj: F[J], fk: F[K], fl: F[L], fm: F[M]): F[(A, B, C, D, E, G, H, I, J, K, L, M)] =
      map12(fa, fb, fc, fd, fe, fg, fh, fi, fj, fk, fl, fm)((_, _, _, _, _, _, _, _, _, _, _, _))
}
