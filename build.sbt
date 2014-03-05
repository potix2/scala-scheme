name := "scheme"

organization := "com.potix2"

version := "0.0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core"   % "7.0.5",
  "org.scalaz" %% "scalaz-effect" % "7.0.5",
  "org.specs2" %% "specs2"        % "2.3.7" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

// Read here for optional dependencies:
// http://etorreborre.github.io/specs2/guide/org.specs2.guide.Runners.html#Dependencies

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

initialCommands := "import com.potix2.scheme._"
