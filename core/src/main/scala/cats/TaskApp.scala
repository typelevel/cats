package cats

/** `App`-like trait that runs a computation in [[Task]]. */
trait TaskApp {
  def run(args: List[String]): Task[Unit]

  final def main(args: Array[String]): Unit =
    run(args.toList).run
}
