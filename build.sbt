import Dependencies._

lazy val commonSettings = Seq(
  organization := "net.kinetc",
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.11.12", "2.12.6"),
  version := "1.0.0-SNAPSHOT",
  description := "NamuMark AST Parser / HTML Transcompiler for MDict",
  name := "biryo",
  test in assembly := {}
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    inThisBuild(List(
      mainClass in assembly := Some("net.kinetc.biryo.MainApp"),
      assemblyJarName in assembly := "biryo.jar",
      javacOptions ++= Seq(
        "-encoding", "UTF-8",
        "-Dfile.encoding", "UTF-8",
        "-Xlint:unchecked",
        "-Xlint:deprecation"),
      scalacOptions ++= List(
        "-encoding", "UTF-8",
        "-feature",
        "-unchecked",
        "-deprecation",
        "-Xlint",
        "-language:_",
        "-Xlog-reflective-calls")
    )),
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
    ),
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.spire-math" %% "jawn-parser" % "0.12.1",
      "org.spire-math" %% "jawn-ast" % "0.12.1",
      "com.typesafe.akka" %% "akka-actor" % "2.5.12",
      "org.parboiled" %% "parboiled" % "2.1.4",
      "org.specs2" %% "specs2-core" % "3.8.9" % "test"
    ),

    scalacOptions in Test ++= Seq("-Yrangepos")
  )