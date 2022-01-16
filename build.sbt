val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tyes",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    ),

    fork := true,
  )
