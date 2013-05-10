name := "Open Neuro-Evolution"
 
version := "1.0"
 
scalaVersion := "2.10.1"
 
scalacOptions += "-deprecation"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test" 

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.2-M3"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-testkit" % "2.2-M3"
