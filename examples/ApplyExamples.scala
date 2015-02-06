package cats
package example

// Apply extends the Functor typeclass (which features the familiar
// "map" function) with a new function "apply".  The apply function
// is similar to map in that we are transforming a value in a context,
// e.g. F[A] where F is the context (e.g. Option, List, Future) and A
// is the type of the value.  But the function A => B is now in the 
// context itself, e.g. F[A => B] such as Option[A => B] or List[A => B].

object ApplyExamples extends App {

  val intToString: Int => String = _.toString
  val double: Int => Int = _ * 2
  val addTwo: Int => Int = _ + 2

  implicit val optionApply: Apply[Option] = new Apply[Option] {
    def apply[A, B](fa: Option[A])(f: Option[A => B]): Option[B] =
      fa.flatMap (a => f.map (ff => ff(a)))

    def map[A,B](fa: Option[A])(f: A => B) = fa map f
  }

  implicit val listApply: Apply[List] = new Apply[List] {
    def apply[A, B](fa: List[A])(f: List[A => B]): List[B] =
      fa.flatMap (a => f.map (ff => ff(a)))

    def map[A,B](fa: List[A])(f: A => B) = fa map f
  }

  // familar uses of map
  assert(Apply[Option].map(Some(1))(intToString) == Some("1"))
  assert(Apply[Option].map(Some(1))(double) == Some(2))
  assert(Apply[Option].map(None)(double) == None)

  // and now ... apply:
  assert(Apply[Option].apply(Some(1))(Some(intToString)) == Some("1"))
  assert(Apply[Option].apply(Some(1))(Some(double)) == Some(2))
  assert(Apply[Option].apply(None)(Some(double)) == None)
  assert(Apply[Option].apply(Some(1))(None) == None)
  assert(Apply[Option].apply(None)(None) == None)

  // Apply's apply function made it possible to build useful functions that
  // "lift" a function that takes multiple arguments into a context.

  // For example,
  val add2 = (a: Int, b: Int) => a + b
  assert(Apply[Option].apply2(Some(1), Some(2))(Some(add2)) == Some(3))

  // Interestingly, if any of the arguments of this example are None, the
  // final result is None.  The effects of the context we are operating on
  // are carried through the entire computation.
  assert(Apply[Option].apply2(Some(1), None)(Some(add2)) == None)
  assert(Apply[Option].apply2(Some(1), Some(2))(None) == None)

  // Like Functors, Apply instances also compose:
  val listOpt = Apply[List] compose Apply[Option]
  val plusOne = (x:Int) => x + 1
  assert(listOpt.apply(List(Some(1), None, Some(3)))(List(Some(plusOne))) == List(Some(2), None, Some(4)))
}
