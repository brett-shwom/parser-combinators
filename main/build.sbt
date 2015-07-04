val _scalaVersion = "2.11.7"

scalaVersion := _scalaVersion

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "2.2.4" % "test",
	"org.scala-lang" % "scala-compiler" % "2.11.7"
)