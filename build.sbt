name := "radical-cadence"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= {
  val akkaV = "2.3.9"
  val sprayV = "1.3.3"

  Seq(
    "de.sciss" %% "scalamidi" % "0.2.0",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "org.scalactic" %% "scalactic" % "2.2.6",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"            %%  "spray-testkit" % sprayV  % "test",
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test"
  )
}

scalacOptions := Seq("-feature", "-deprecation", "-encoding", "utf8")

initialCommands in console := "import rc.dsl._, rc.dsl.Structures._, rc.dsl.parser._, rc.dsl.Primitives._"
