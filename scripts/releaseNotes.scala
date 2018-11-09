import scala.io.Source

/**
 * A script to create release notes from github pull request page.
 * To use first go to https://github.com/typelevel/cats/pulls?q=is%3Apr+milestone%3A_MILESTONE_+is%3Aclosed
 * replace _MILESTONE_ with actual milestone like 1.5
 * Make sure all PR are labeled properly with one of the build|enhancement|documentation|testing|bug
 * copy the content of the table from each page to a file.
 * and run `scala releaseNotes.scala YOUR_FILE_NAME``
 */
object ReleaseNotesFromGitHubText {
  def main(args: Array[String]): Unit = {
    val cleaned = Source.fromFile(args(0)).getLines().toList
      .filterNot(_.trim.isEmpty)
      .filterNot(_.matches("^\\s+\\d+\\s*$")) //remove comments count
      .map(_.replaceAll(""" was merged .+""", ""))
      .sliding(2).zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

    val titleReg = "(.+)\\s+(build|enhancement|documentation|testing|bug)$$".r
    val numUserReg = """#(\d+)\sby\s(.+)$""".r

    val prs = cleaned.map {
      case List(titleReg(title, label), numUserReg(num, user)) => PR(title, label, num, user)
    }.toList

    def toTitle(lable: String): String = lable match {
      case "build" => "build improvements"
      case "testing" => "test improvments"
      case "bug" => "bug fixes"
      case "documentation" => "documentation additions/fixes"
      case "enhancement" => "API/Feature enhancements"
    }

    val out = prs.groupBy(_.label).map {
      case (label, prs) =>
        s"""
          |### ${prs.size} ${toTitle(label)}
          |
          |${prs.map(_.toMD).mkString("\n|")}
        """.stripMargin
    }.mkString("\n")

    println(out)
  }

  case class PR(title: String, label: String, num: String, user: String) {

    def toMD: String =
     s"* [#$num](https://github.com/typelevel/cats/pull/$num) $title by @$user"
  }

}
