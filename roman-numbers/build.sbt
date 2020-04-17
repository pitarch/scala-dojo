lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.example",
      scalaVersion := "2.13.1"
    )
  ),
  name := "scalatest-example"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test

triggeredMessage in ThisBuild := Watched.clearWhenTriggered

// https://gist.github.com/michaeldfallen/712ff643c285a7d378be
