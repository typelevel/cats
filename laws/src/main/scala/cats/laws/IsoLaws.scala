package cats.laws

import cats.Iso

/** Laws that must be obeyed by any Isomorphism */
trait IsoLaws {
  def isoIdentityTo[->[_,_], A, B](iso: Iso[->, A, B]) =
    iso.cat.andThen(iso.to, iso.from) <-> iso.cat.id[A]

  def isoIdentityFrom[->[_,_], A, B](iso: Iso[->, A, B]) =
    iso.cat.andThen(iso.from, iso.to) <-> iso.cat.id[B]
}
