package cats
package instances


trait HashInstances extends kernel.HashToHashingConversion {

  implicit val catsContravariantForHash: Contravariant[Hash] =
    new Contravariant[Hash] {
      /**
       * Derive a `Hash` for `B` given an `Hash[A]` and a function `B => A`.
       */
      def contramap[A, B](ha: Hash[A])(f: B => A): Hash[B] = Hash.by(f)(ha)

    }

}
