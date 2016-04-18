package cats
package tests

import cats.Applicative
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.Await

class ApplicativeCheck extends CatsSuite {

  test("replicateA creates a List of 'n' copies of given Applicative 'fa'") {

    val A = Applicative[Option]
    val fa = A.pure(1)
    A.replicateA(5, fa) should === (Some(List(1,1,1,1,1)))
  }

  test("replicateA evaluates the 'fa' argument by-name") {
    import scala.concurrent.ExecutionContext.Implicits.global

    val timeout = 5.seconds
    val repetitions = 5
    val A = Applicative[Future]

    @volatile var n: Int = 0
    def fut(): Future[Unit] = A.pure(n += 1)

    def program: Future[List[Unit]] = A.replicateA(repetitions, fut())
    Await.result(program, timeout)

    n should === (repetitions)
  }
}
