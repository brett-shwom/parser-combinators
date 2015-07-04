lazy val mainProject = Project("main", file("main")) dependsOn(macroProject)
lazy val macroProject = Project("macro", file("macro"))

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
