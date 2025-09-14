val scala3Version = "3.7.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tyes",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
    ),

    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.16",
      "org.scalatest" %% "scalatest" % "3.2.16" % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    ),

    fork := true,
    connectInput := true,
    outputStrategy := Some(OutputStrategy.StdoutOutput),
  )
