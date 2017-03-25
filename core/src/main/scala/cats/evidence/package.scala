package cats

package object evidence {
  type Leibniz[A, B] = cats.evidence.Is[A, B]
}
