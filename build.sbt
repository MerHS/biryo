import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "net.kinetc",
      scalaVersion := "2.11.8",
      crossScalaVersions := Seq("2.11.8", "2.12.1"),
      version      := "0.1.0-SNAPSHOT",
      description := "NamuMark AST Parser / HTML Transcompiler for MDict",
      javacOptions ++= Seq(
        "-encoding", "UTF-8",
        "-source", "1.6",
        "-target", "1.6",
        "-Xlint:unchecked",
        "-Xlint:deprecation"),
      scalacOptions ++= List(
        "-encoding", "UTF-8",
        "-feature",
        "-unchecked",
        "-deprecation",
        "-Xlint",
        "-language:_",
        "-target:jvm-1.6",
        "-Xlog-reflective-calls")
    )),
    name := "MainApp",
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
    ),
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.spire-math" %% "jawn-parser" % "0.10.4",
      "org.spire-math" %% "jawn-ast" % "0.10.4",
      "com.typesafe.akka" %% "akka-actor" % "2.5.1",
      "org.parboiled" %% "parboiled" % "2.1.4",
      "org.specs2" %% "specs2-core" % "3.8.9" % "test"
    ),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
