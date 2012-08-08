name := "scalacolt"

version := "0.0.1"

organization := "org.barbers"

scalaVersion := "2.9.2"

// Use ScalaCheck
resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "colt" % "colt" % "1.2.0",
  "org.scalacheck" %% "scalacheck" % "1.10.0" % "test",
  "org.specs2" % "specs2_2.9.2" % "1.11" % "test"
)

parallelExecution in Test := true
