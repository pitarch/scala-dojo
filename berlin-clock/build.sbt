lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.13.1"
    )),
    name := "berlin-clock-kata"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test
