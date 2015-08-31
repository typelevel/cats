package cats
package tests

class TaskTests extends CatsSuite {

  test("side-effects aren't run until .run is called") {
    var run: Int = 0
    val t: Task[Int] = Task.later { run += 1; run }
    assert(run == 0)
    t.run
    assert(run == 1)
  }

  test("side-effects are only evaulated once for Task.later") {
    var run: Int = 0
    val t: Task[Int] = Task.later { run += 1; run }
    t.run
    assert(run == 1)

    t.run
    assert(run == 1)

    val t2 = t.map(_ + 1)
    t2.run
    t2.run

    assert(run == 1)

    val t3 = t.flatMap(x => Task.now(x + 1))
    t3.run
    t3.run

    assert(run == 1)
  }

  test("async is not run until run is called") {
    var run: Int = 0
    val t: Task[Int] = Task.async { cb => run += 1; cb(1) }
    assert(run == 0)

    t.run
    assert(run == 1)
  }

  test("async is not run multiple times if the task is rum multiple times") {
    var run: Int = 0
    val t: Task[Int] = Task.async { cb => run += 1; cb(1) }
    assert(run == 0)

    t.run
    assert(run == 1)

    t.run
    assert(run == 2)

    val t2 = t.flatMap(x => Task.now(x + 1))
    t2.run
    assert(run == 3)
  }

  test("always not run until run is called") {
    var run: Int = 0
    val t: Task[Int] = Task.always { () => run += 1; 1 }
    assert(run == 0)

    t.run
    assert(run == 1)
  }

  test("always is run multiple times if the task is rum multiple times") {
    var run: Int = 0
    val t: Task[Int] = Task.always { () => run += 1; 1 }
    assert(run == 0)

    t.run
    assert(run == 1)

    t.run
    assert(run == 2)

    val t2 = t.flatMap(x => Task.now(x + 1))
    t2.run
    assert(run == 3)
  }
}
