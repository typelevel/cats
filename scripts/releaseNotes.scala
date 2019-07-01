import scala.io.Source

/**
 * A script to create release notes from github pull request page.
 * To use first go to https://github.com/typelevel/cats/pulls?q=is%3Apr+milestone%3A_MILESTONE_+is%3Aclosed
 * replace _MILESTONE_ with actual milestone like 1.5
 * Make sure all PR are labeled properly with one of the
 * "Binary Breaking", "Source Breaking", "bug", "enhancement", "documentation", "testing", "build"
 * copy the content of the table from each page to a file (see releaseNotesTestFile as an example).
 * and run `scala releaseNotes.scala YOUR_FILE_NAME``
 */
object ReleaseNotesFromGitHubText {
  def main(args: Array[String]): Unit = {
    val cleaned = Source.fromFile(args(0)).getLines().toList
      .filterNot(_.trim.isEmpty)
      .filterNot(_.matches("^\\s+\\d+\\s*$")) //remove comments count
      .map(_.replaceAll(""" was merged .+""", ""))
      .sliding(2).zipWithIndex.filter(_._2 % 2 == 0).map(_._1)

    val lables = List("Binary Breaking", "Source Breaking", "bug", "enhancement", "documentation", "testing", "build" )
    val normalReg = ("(.+)\\s+("+ lables.drop(2).mkString("|") + ")$$").r
    val numUserReg = """#(\d+)\sby\s(.+)$""".r

    val binaryBreakingReg = ("(.+)\\s+("+ lables.head +").*$$").r
    val sourceBreakingReg = ("(.+)\\s+("+ lables(1) +").*$$").r

    val prs = cleaned.map {
      case List(binaryBreakingReg(title, label), numUserReg(num, user)) => PR(title, label, num, user)
      case List(sourceBreakingReg(title, label), numUserReg(num, user)) => PR(title, label, num, user)
      case List(normalReg(title, label), numUserReg(num, user)) => PR(title, label, num, user)
    }.toList

    def toTitle(lable: String, size: Int): String = {
      val singular =
        lable match {
          case "build" => "build improvement"
          case "testing" => "test improvement"
          case "bug" => "bug fix"
          case "documentation" => "documentation improvement"
          case "enhancement" => "API/feature enhancement"
          case "Source Breaking" => "source breaking change"
          case "Binary Breaking" => "binary breaking change"
        }

      //poor man's pluralizer
      if(size > 1) {
        if(singular.endsWith("x")) singular + "es"
        else singular + "s"
      } else singular

    }

    val out = prs.groupBy(_.label).toList.sortBy(p => lables.indexOf(p._1)).map {
      case (label, prs) =>
        s"""
          |### ${prs.size} ${toTitle(label, prs.size)}
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
