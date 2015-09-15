resolvers += Resolver.url(
  "tpolecat-sbt-plugin-releases",
    url("http://dl.bintray.com/content/tpolecat/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

addSbtPlugin("com.eed3si9n"      % "sbt-unidoc"             % "0.3.2")
addSbtPlugin("com.github.gseitz" % "sbt-release"            % "1.0.0")
addSbtPlugin("com.jsuereth"      % "sbt-pgp"                % "1.0.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-ghpages"            % "0.5.3")
addSbtPlugin("com.typesafe.sbt"  % "sbt-site"               % "0.8.1")
addSbtPlugin("org.tpolecat"      % "tut-plugin"             % "0.4.0")
addSbtPlugin("pl.project13.scala"% "sbt-jmh"                % "0.2.3")
addSbtPlugin("org.scalastyle"    %% "scalastyle-sbt-plugin" % "0.6.0")
addSbtPlugin("org.scoverage"     % "sbt-scoverage"          % "1.2.0")
addSbtPlugin("com.typesafe.sbt"  % "sbt-git"                % "0.8.4")
addSbtPlugin("org.scala-js"      % "sbt-scalajs"            % "0.6.5")
