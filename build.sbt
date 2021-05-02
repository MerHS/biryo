import Dependencies._

lazy val commonSettings = Seq(
  organization := "net.kinetc",
  scalaVersion := "2.13.5",
  crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.5"),
  version := "1.4.0",
  description := "NamuMark AST Parser / HTML Transcompiler for MDict",
  name := "biryo",
  assembly / test := {}
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    inThisBuild(
      List(
        assembly / mainClass := Some("net.kinetc.biryo.MainApp"),
        assembly / assemblyJarName := "biryo.jar",
        javacOptions ++= Seq(
          "-encoding",
          "UTF-8",
          "-Dfile.encoding",
          "UTF-8",
          "-Xlint:unchecked",
          "-Xlint:deprecation"
        ),
        scalacOptions ++= List(
          "-encoding",
          "UTF-8",
          "-feature",
          "-unchecked",
          "-deprecation",
          "-Xlint",
          "-language:_",
          "-Xlog-reflective-calls"
        )
      )
    ),
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
    ),
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.typelevel" %% "jawn-parser" % "1.0.0",
      "org.typelevel" %% "jawn-ast" % "1.0.0",
      "com.typesafe.akka" %% "akka-actor-typed" % "2.6.14",
      "org.parboiled" %% "parboiled" % "2.3.0",
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.specs2" %% "specs2-core" % "4.11.0" % "test"
    ),
    test / scalacOptions ++= Seq("-Yrangepos")
  )
