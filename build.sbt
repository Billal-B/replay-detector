name := "ReplayDetector"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"

// https://mvnrepository.com/artifact/com.amazonaws/aws-java-sdk
libraryDependencies += "com.amazonaws" % "aws-java-sdk" % "1.11.534"
// https://mvnrepository.com/artifact/commons-io/commons-io
libraryDependencies += "commons-io" % "commons-io" % "2.4"

// https://mvnrepository.com/artifact/com.google.cloud/google-cloud-storage
libraryDependencies += "com.google.cloud" % "google-cloud-storage" % "1.85.0"

libraryDependencies += "com.typesafe.akka" %% "akka-http"   % "10.1.9"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.23"
libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.9"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.4"


assemblyMergeStrategy in assembly := {
  case PathList("reference.conf") => MergeStrategy.concat
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case x => MergeStrategy.first
}