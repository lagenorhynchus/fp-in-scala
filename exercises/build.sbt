lazy val root = (project in file("."))
  .settings(
    name := "exercise",
    version := "0.1.0",
    scalaVersion := "2.12.2",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")
  )
