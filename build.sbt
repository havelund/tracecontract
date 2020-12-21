name := "TraceContract"

version := "1.0"

// Scala:

scalaVersion := "2.13.0"

// Akka:

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.23",
  "junit" % "junit" % "4.11" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test
        exclude("junit", "junit-dep")
)

