package cats
package example

/**
  * A Functor is a ubiquitous typeclass involving type constructors of
  * kind * → *, which is another way of saying types that have a
  * single type variable. Examples might be Option, List, Future.
  *
  * The Functor category involves a single operation, named `map`:
  *
  * def map[A, B](fa: F[A])(f: A => B): F[B]
  *
  * This method takes a Function from A => B and turns an F[A] into an F[B]
  */
object FunctorExamples extends App {

  val len: String => Int = _.length

  //
  // example instances Instances
  //

  // we can trivially create a functor instance for a type which has a
  // well behaved map method;
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A,B](fa: Option[A])(f: A => B) = fa map f
  }
  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A,B](fa: List[A])(f: A => B) = fa map f
  }

  //
  // map
  //

  // Option is a functor which always returns a Some with the function
  // applied when the Option value is a Some.
  assert(Functor[Option].map(Some("adsf"))(len) == Some(4))
  // When the Option is a None, it always returns None
  assert(Functor[Option].map(None)(len) == None)

  // List is a functor which applies the function to each element of
  // the list.
  assert(Functor[List].map(List("qwer", "adsfg"))(len) == List(4,5))

  //
  // lift
  //

  // We can use the Funtor to "lift" a function to operate on the Functor type:
  val lenOption: Option[String] => Option[Int] = Functor[Option].lift(len)
  assert(lenOption(Some("abcd")) == Some(4))

  //
  // fproduct
  //

  // Functor provides a fproduct function which pairs a value with the
  // result of applying a function to that value.
  val source = List("a", "aa", "b", "ccccc")
  val result = Map("a" -> 1, "aa" -> 2, "b" ->  1, "ccccc" -> 5)

  assert(Functor[List].fproduct(source)(len).toMap == result)


  //
  // Composition
  //

  // Functors compose! Given any Functor F[_] and any Functor G[_] we
  // can compose the two Functors to create a new Functor on F[G[_]]:
  val listOpt = Functor[List] compose Functor[Option]
  assert(listOpt.map(List(Some(1), None, Some(3)))(_ + 1) == List(Some(2), None, Some(4)))
}
