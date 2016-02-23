package cats

abstract class TaskApp {
  def run(args: Vector[String]): Task[Unit]

  final def main(args: Array[String]): Unit =
    run(Vector(args:_*)).unsafePerformIO()
}
