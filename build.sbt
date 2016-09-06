name := """logicCompiler"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.tinyjee.jgraphx" % "jgraphx" % "1.10.1.3"
)

enablePlugins(JavaAppPackaging)

mainClass in Compile := Some("ui.Main")
