val _scalaVersion = "2.11.7"

scalaVersion := _scalaVersion

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % _scalaVersion
)